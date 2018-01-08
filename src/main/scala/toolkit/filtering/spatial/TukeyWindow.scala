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
import scala.math.{cos, Pi}

/** Implements a Tukey window.
  *
  * See http://en.wikipedia.org/wiki/Window_function
  *
  * @author Greg Snider
  */
object TukeyWindow {

  /** Create a Tukey window.
    *
    * @param rows Number of rows in window.
    * @param columns Number of columns in window.
    * @param alpha Number specifying width of flat part: 0 => all flat, 1 =>
    *        Hann window, 0.5 => half flat.
    * @return Multiplicative Tukey window as a matrix.
    */
  def apply(rows: Int, columns: Int, alpha: Double): Matrix = {
    val window = new Matrix(rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)  {
      val rowFactor = w(row, rows, alpha)
      val colFactor = w(col, columns, alpha)
      window(row, col) = rowFactor * colFactor
    }
    window
  }

  private def w(n: Int, N: Int, alpha: Double): Float = {
    val lowerBound = alpha * (N - 1) / 2
    val upperBound = (N - 1) * (1 - alpha / 2)
    if (n < lowerBound) {
      val arg = ((2.0 * n) / (alpha * (N - 1))) - 1
      0.5f * (1.0 + cos(Pi * arg)).toFloat
    } else if (n > upperBound) {
      val arg = ((2.0 * n) / (alpha * (N - 1))) + 1 - (2.0 / alpha)
      0.5f * (1.0 + cos(Pi * arg)).toFloat
    } else
      1.0f
  }
}
