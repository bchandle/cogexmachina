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

/** Factory for creating complex filters (ComplexScalarFields) from a function
  * that generates a complex number at a point given the polar coordinates of
  * that point (r, theta).
  *
  * @author Greg Snider
  */
object ComplexPolarFilter {
  /** Create a complex field from a function that supplies values given
    * polar coordinates.
    *
    * @param size Half-width and half-height of the kernel. The kernel
    *        logically extends from `-size` to `size` along
    *        the x-axis, and from `-size` to `size` along the y-axis.
    * @param samples Number of discrete samples along each axis for the kernel.
    * @param polar Function which specifies the values of the field as a
    *        function of (magnitude, phase).
    * @return The kernel as a complex field.
    */
  def apply(size: Float, samples: Int, polar: (Float, Float) => Complex): ComplexField = {
    require(size > 0, "Monopole field size must positive, non-zero.")
    require(samples % 2 == 1, "Monopole field samples must be odd.")
    val center = samples / 2
    // Scale factor: sampled coordinates --> logical coordinates
    val scale = size / center
    // Provide value of the sampled field at cartesian location ("row", "col").
    def cartesian(row: Int, col: Int): Complex = {
      val x = (col - center) * scale
      val y = (row - center) * scale
      val r = math.sqrt(x * x + y * y).toFloat
      val theta = math.atan2(y, x).toFloat
      polar(r, theta)
    }
    ComplexField(samples, samples, cartesian)
  }
}