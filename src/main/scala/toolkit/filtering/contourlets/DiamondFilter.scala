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

package toolkit.filtering.contourlets

import libcog._
import toolkit.filtering.MatlabFunctions

/** Directional filters.
  *
  *
  * Implements dfilters.m (although only one of the filters).
  *
  * @author Greg Snider
  */
object DiamondFilter extends MatlabFunctions {

  /** Lowpass, highpass analysis diamond filters. */
  lazy val (lowpass, highpass) = dmaxflat7(true)

  /** Lowpass, highpass synthesis diamond filters. */
  lazy val (inverseLowpass, inverseHighpass) = dmaxflat7(false)

  /** Create a (highpass, lowpass) pair of dmaxflat7 diamond filters.
    *
    * @param decomposition True => decomposition, false => reconstruction.
    * @return (highpass, lowpass) diamond filters.
    */
  private def dmaxflat7(decomposition: Boolean = true): (Matrix, Matrix) = {
    def sqrt(x: Float) = math.sqrt(x).toFloat
    val M1 = 1f / sqrt(2)
    val M2 = M1
    val k1 = 1 - sqrt(2)
    val k3 = k1
    val k2 = M1

    var h = Matrix(
      Array(.25f * k2 * k3, .5f * k2, 1f +.5f * k2 * k3)
    ) * M1

    h = concat(h, fliplr(clipLastColumn(h)))
    var g = Matrix(
      Array(-0.125f * k1 * k2 * k3, 0.25f * k1 * k2,
        (-0.5f * k1 -0.5f * k3 - 0.375f * k1 * k2 * k3), 1 + 0.5f * k1 * k2)
    ) * M2

    g = concat(g, fliplr(clipLastColumn(g)))
    val B  = DiamondMaxFlatFilter(7, 0)

    var h0 = McClellanTransform(h, B)
    var g0 = McClellanTransform(g, B)

    val h0sum = h0.reduce(_ + _)
    h0 = (h0 / h0sum) * sqrt(2)
    val g0sum = g0.reduce(_ + _)
    g0 = (g0 / g0sum) * sqrt(2)

    var h1 = Modulate.rowsAndColumns(g0)

    if (!decomposition) {
      h1 = Modulate.rowsAndColumns(h0)
      h0 = g0
    }
    (h0, h1)
  }
}
