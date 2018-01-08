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

/** A Stick filter is a 2-dimensional Gaussian with different sigmas for each
  * dimension. The constructor also allows the stick to be offset radially from
  * the center, in the direction perpendicular to the "long" direction of the
  * kernel, and to be rotated. The size of the kernel is computed automatically.
  *
  * @author Greg Snider
  */
object StickFilter {
  /** Create a horizontal stick filter rotated to `angle` with sigma in the
    * horizontal direction of `horizSigma`, sigma in the vertical direction
    * of `vertSigma` (which will normally be smaller than `horizSigma`) and
    * offset from the horizontal axis, `shift`.
    */
  def apply(angle: Float, horizSigma: Float, vertSigma: Float,
            shift: Float): ScalarField =
  {
    val kernel = asMatrix(angle, horizSigma, vertSigma, shift)
    ScalarField(kernel.rows, kernel.columns, (row, col) => kernel(row, col))
  }


  /** Create a horizontal stick filter rotated to `angle` with sigma in the
    * horizontal direction of `horizSigma`, sigma in the vertical direction
    * of `vertSigma` (which will normally be smaller than `horizSigma`) and
    * offset from the horizontal axis, `shift`.
    */
  def asMatrix(angle: Float, horizSigma: Float, vertSigma: Float,
            shift: Float): Matrix =
  {
    val size = GaussianFilter.bestSize(horizSigma max vertSigma)
    val offset = size / 2
    val kernel = new Matrix(size, size)
    import scala.math.{cos, sin, exp, Pi}
    for (i <- 0 until size; j <- 0 until size) {
      // Rotate the filter
      val c = cos(angle)
      val s = sin(angle)
      // Make coordinates relative to center
      val y = (i - offset) + c * shift
      val x = (j - offset) + s * shift
      // Compute the two Gaussians
      val term1 = -0.5 * sq((x * c - y * s) / horizSigma)
      val term2 = -0.5 * sq((x * s + y * c) / vertSigma)
      val normalize = 1 / (2 * Pi * horizSigma * vertSigma)
      val point = normalize * exp(term1 + term2)
      kernel(i, j) =  point.toFloat
    }
    kernel
  }

  private def sq(a: Double) = a * a
}

/** An example of Stick filters with equal angular spread. */
object StickFilterExample {
  import scala.math.Pi
  val Orientations = 12
  def angle(index: Int): Float = ((index * 2 * Pi) / Orientations).toFloat
  val LongSigma = 3f
  val ShortSigma = 1f
  val VerticalShift = 1f
  val kernels = Array.tabulate[ScalarField](Orientations) {
    (orient) => {
      val kernel = StickFilter(angle(orient), LongSigma, ShortSigma, VerticalShift)
      require(kernel != null)
      kernel
    }
  }
}