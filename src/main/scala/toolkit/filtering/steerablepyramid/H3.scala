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

package toolkit.filtering.steerablepyramid

import libcog._

/** Implements the 3rd order odd kernel for the steerable pyramid. This
  * comes from Tables I and II of "Design and Use of Steerable Filters",
  * Freeman and Adelson.
  *
  * A comment in their Matlab code says these old filters from their 1991
  * paper are not very accurate, but their newer, more accurate, filter generator
  * code was not included in the distribution.
  *
  * The value of `Scale` in the code was determined empirically by minimizing
  * the mean square error beteen the old G3 filters from 1991 paper and the new
  * G3 filters in their Matlab code. This is not too important here since we're
  * not using this G3 kernel in the pyramid, but it helped us calibrate the
  * crummy H3 filter here which we are using.
  *
  * @author Greg Snider
  */
object H3 {
  private val Kernels = 4
  private val Scale = 1.351f

  /** Create an H3 kernel.
    *
    * @param kernelIndex Index of kernel in the G3 basis set.
    * @param kernelSize Size (height and width) of kernel.
    *
    */
  def apply(kernelIndex: Int, kernelSize: Int = 9): ScalarField = {
    require(kernelSize % 2 == 1, "kernel size must be odd")
    val Offset = kernelSize / 2
    val matrix = new Matrix(kernelSize, kernelSize)
    require(kernelIndex >= 0 && kernelIndex < Kernels)
    for (row <- 0 until kernelSize) {
      for (col <- 0 until kernelSize) {
        val y = (col - Offset) / Scale
        val x = (row - Offset) / Scale
        val (xx, yy) = rotate(x, y, angle(kernelIndex))
        matrix(row, col) = amplitude(xx, yy)
      }
    }
    ScalarField(kernelSize, kernelSize, (row, col) => matrix(row, col)).normalizeL2 * 0.5f
  }

  import scala.math.{cos, sin, exp}

  /** Rotate vector (`x`, `y`) by `angle`. */
  private def rotate(x: Float, y: Float, angle: Float): (Float, Float) = {
    val xRotated = x * cos(angle) - y * sin(angle)
    val yRotated = x * sin(angle) + y * cos(angle)
    (xRotated.toFloat, yRotated.toFloat)
  }

  /** Amplitude at location (`x`, `y`) from Table II. */
  private def amplitude(x: Float, y: Float): Float =
    (-0.9454f + 2.959f * x * x - 0.6582f * x * x * x * x) * exp(-(x * x + y * y)).toFloat

  /** Angle for kernel `index`, from caption on Table I. */
  private def angle(index: Int): Float =
    (index * math.Pi).toFloat / Kernels
}
