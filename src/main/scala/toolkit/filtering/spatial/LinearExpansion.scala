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

package toolkit.filtering.spatial

import libcog._

/** Does a local polynomial expansion of a 2D image assuming constant
  * certainty and a Gaussian applicability. See "Polynomial expansion for
  * orientation and motion estimation," Farneback, section 4.8, equations
  * 4.30
  *
  * @author Greg Snider
  */
object LinearExpansion extends FloatMath {
  /** Width of Gaussian. */
  val Sigma = 1.5f
  /** Size of each filter. */
  val FilterSize = 11
  val FilterPixels = FilterSize * FilterSize
  /** Gaussian applicability function. */
  val applicability = new Matrix(FilterSize, FilterSize) {
    val Offset = rows / 2
    val SigmaSq = Sigma * Sigma
    for (r <- -Offset to Offset; c <- -Offset to Offset) {
      val row = r + Offset
      val col = c + Offset
      def gauss(r: Int, c: Int): Float = exp(-(r * r + c * c) / (2 * SigmaSq))
      this(row, col) = gauss(r, c)
    }
  }
  /** Polynomial filters. */
  val filters = new LinearBasis(applicability)
  /** Analysis filters, stacked into a vector field. */
  val analysisOperators = VectorField(FilterSize, FilterSize,
    (row, col) =>
      new Vector(
        filters.Gdc(row, col),
        filters.Grow(row, col),
        filters.Gcol(row, col)
      )
  )
  /** Synthesis filters, stacked into a vector field. */
  lazy val synthesisOperators = VectorField(FilterSize, FilterSize,
    (row, col) =>
      new Vector(
        filters.Bdc(row, col),
        filters.Brow(row, col) / FilterPixels,
        filters.Bcol(row, col) / FilterPixels
      )
  )

  /** Project an image into a polynomial expansion with a Gaussian
    * applicability with width 1.
    *
    * @param image The 2D image to be expanded into polynomial.
    */
  def apply(image: ScalarField): (VectorField, ScalarField) = {
    val filtered = image.crossCorrelate(analysisOperators, BorderClamp)
    val b: VectorField = vectorField(
      Array(filtered.vectorElement(1), filtered.vectorElement(2))
    )
    val c: ScalarField = filtered.vectorElement(0)
    (b, c)
  }

  /** Adjoint operator for apply, reconstructs original image from the
    * polynomial expansion.
    *
    * @param b Linear term of projection.
    * @param c Constant term of projection.
    * @return Reconstructed image.
    */
  def backproject(b: VectorField, c: ScalarField): ScalarField =
  {
    val dc = c.convolve(
      synthesisOperators.vectorElement(0), BorderClamp)
    val br = b.vectorElement(0).convolve(
      synthesisOperators.vectorElement(1), BorderClamp)
    val bc = b.vectorElement(1).convolve(
      synthesisOperators.vectorElement(2), BorderClamp)
    val projection = (dc + br + bc) / FilterPixels
    projection
  }
}

