/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package toolkit.filtering

import libcog._
import scala.collection.mutable.HashMap

/** Implements Moisan's periodic FFT. This is modeled on Peter Kovesi's
  * Matlab implementation of the algorithm. See the paper "Periodic plus
  * Smooth Image Decomposition," Journal of Mathematical Imaging and Vision,
  * vol 39:2, pp. 161-179, 2011.
  *
  * The input is an image that is suitable for the FFT (number of rows and
  * columns are powers of 2). The output is the FFT of the "periodic"
  * component of the input image with border effects suppressed.
  *
  * NOTE: This could be sped up by writing a GPU kernel for
  * `computeBoundaryImage`.
  *
  * @author Greg Snider
  */
object PeriodicFFT extends Logarithm with FloatMath {
  private val normalizerCache = new HashMap[Shape, ScalarField]

  /** Compute the "periodic" FFT of an image. This suppresses border effects
    * with minimal impact on filters.
    *
    * @param image The input image to be transformed with the periodic FFT.
    * @return The transformed image in the frequency domain of the (periodic,
    *         smooth) components.
    */
  def apply(image: ScalarField): (ComplexField, ComplexField) = {
    require(image.fieldShape.dimensions == 2, "periodicFFT requires 2D input")
    require(image.tensorShape.dimensions == 0, "periodicFFT requires scalar input")
    val rows = image.fieldType.rows
    val columns = image.fieldType.columns
    require(isPowerOf2(rows), "input image rows must be power of 2")
    require(isPowerOf2(columns), "input image columns must be power of 2")
    val boundaryImage = computeBoundaryImage(image)

    // Generate FFT of smooth component
    val smooth = boundaryImage.fft * getSmoothNormalizer(image.fieldShape)

    // Now generate the periodic component
    val periodic = image.fft - smooth
    (periodic, smooth)
  }

  /** Get a multiplicative normalizer for calculating the FFT of the
    * smooth component.
    *
    * @param fieldShape The shape of the image being transformed.
    * @return Multiplicative normalizer.
    */
  private def getSmoothNormalizer(fieldShape: Shape): ScalarField = {
    if (!normalizerCache.contains(fieldShape)) {
      // Cache miss. Create the normalizer.
      val rows = fieldShape(0)
      val columns = fieldShape(1)
      val normalizer = ScalarField(rows, columns,
        (row, col) => {
          if (row == 0 && col == 0)
            0f  // Force 0 mean
          else {
            val cx = cos(2 * Pi * row / rows).toFloat
            val cy = cos(2 * Pi * col / columns).toFloat
            1f / (2 * (2 - cx - cy))
          }
        }
      )
      normalizerCache(fieldShape) = normalizer
    }
    normalizerCache(fieldShape)
  }

  /** Compute the boundary image as part of the task of constructing the
    * smooth component. This should be made into a GPU kernel for speed.
    */
  object computeBoundaryImage extends Operator {
    def compute(in: ScalarFieldReader, out: ScalarFieldWriter) {
      out.setShape(in.fieldShape)
      // Initialize output to zero.
      for (row <- 0 until in.rows; col <- 0 until in.columns)
        out.write(row, col, 0f)

      // Write top and bottom edges, except for corners.
      for (col <- 1 until in.columns - 1) {
        val top = in.read(0, col) - in.read(in.rows - 1, col)
        val bottom = -top
        out.write(0, col, top)
        out.write(in.rows - 1, col, bottom)
      }

      // Write left and right edges, except for corners.
      for (row <- 1 until in.rows - 1) {
        val left = in.read(row, 0) - in.read(row, in.columns - 1)
        val right = -left
        out.write(row, 0, left)
        out.write(row, in.columns - 1, right)
      }

      // Read the corner pixels
      val nw = in.read(0, 0)
      val ne = in.read(0, in.columns - 1)
      val sw = in.read(in.rows - 1, 0)
      val se = in.read(in.rows - 1, in.columns - 1)

      // Write out the corners
      out.write(0, 0, 2 * nw - ne - sw)
      out.write(in.rows - 1, 0, 2 * sw - se - nw)
      out.write(0, in.columns - 1, 2 * ne - nw - se)
      out.write(in.rows - 1, in.columns - 1, 2 * se - ne - sw)
    }
  }
}