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

/**
  * The LanczosFilter is used for resampling discrete images, especially
  * upsampling, to interpolate missing values. This implementation is used
  * mainly for 2X upsampling, meaning that an M x N image is interpolated to
  * a 2M x 2N image. To use this kernel, convert the M x N image to 2M x 2N
  * by inserting zeroes in the unsampled points, then convolving that with
  * this kernel. Higher upsampling is also possible (below).
  *
  * The filter has a parameter, a, which implicitly defines the size of the
  * filter. In practice, only two values of a are used:
  *
  * a      kernel size
  * ------------------
  * 2        7 x 7
  * 3       11 x 11
  *
  * The default value for a is 2 since it is the "best compromise in terms
  * of reduction of aliasing, sharpness, and minimal ringing." See
  * http://en.wikipedia.org/wiki/Lanczos_resampling for a discussion of this.
  *
  * The second parameter, upsampleFactor, defines how much interpolation is
  * to be done. The default value is 2. For a larger value, say K, convert the
  * M x N image to a KM x KN image by inserting zeros and convolving with this
  * kernel.
  *
  * @author Greg Snider
  */

object LanczosFilter {
  /** Create a Lanczos filter.
    *
    * @param a Window width, default value is 2.
    * @param upsampleFactor Linear scaling of input image size to output image
    *                       size, default value is 2.
    */
  def apply(a:Int = 2, upsampleFactor:Int = 2)
    = ScalarField(matrix(a, upsampleFactor))

  /** Create a Lanczos filter as a matrix.
    *
    * @param a Window width, default value is 2.
    * @param upsampleFactor Linear scaling of input image size to output image
    *                       size, default value is 2.
    */
  def matrix(a:Int = 2, upsampleFactor:Int = 2): Matrix = {
    val width = 2 * upsampleFactor * a - 1
    val offset = (width - 1) / 2
    val fcn: (Int,Int) => Float = (r,c) => {
      val x = (r - offset) / upsampleFactor.toDouble
      val y = (c - offset) / upsampleFactor.toDouble
      (L(x, a) * L(y, a)).toFloat
    }
    Matrix(width, width, fcn)
  }


  /** The Lanczos function. */
  private def L(x: Double, a:Int): Double = {
    import math.{sin,Pi}
    if (x == 0)
      1.0
    else
      (a * sin(Pi * x) * sin(Pi * x / a)) / (Pi * Pi * x * x)
  }
}
