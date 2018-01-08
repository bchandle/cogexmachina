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

/** Implements a Blackman window.
  *
  * See http://en.wikipedia.org/wiki/Window_function#Blackman_windows
  *
  * @author Greg Snider
  */
object BlackmanWindow {
  val Alpha = 0.16f
  val A0 = (1 - Alpha) / 2
  val A1 = 0.5f
  val A2 = Alpha / 2

  /** Create a `size` x `size` Blackman window as a 2D scalar field. */
  def apply(size: Int): ScalarField = {
    ScalarField(size, size, (row, col) => window(row, size) * window(col, size))
  }

  private def window(n: Int, windowSize: Int): Float =
    (A0 - A1 * cos((2 * Pi * n) / (windowSize - 1)) +
            A2 * cos((4 * Pi * n) / (windowSize - 1))).toFloat
}
