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

package toolkit.filtering.frequency

import libcog._
import toolkit.filtering.MatlabFunctions


/** Implements Peter Kovesi's filtergrid function from his computer vision
  * library: http://www.csse.uwa.edu.au/~pk/research/matlabfns/
  *
  * @author Greg Snider
  */
object Filtergrid extends Logarithm with MatlabFunctions {

  /** Generates grid for constructing frequency domain filters. See the original
    * filtergrid Matlab code for details. This uses the same naming convention
    * to make it easier to correlate with the original.
    *
    * @param rows Number of rows in filter.
    * @param columns Number of columns in filter.
    *
    * @return Tuple(radius, u1, u2). Radius: Grid of size [rows cols]
    *         containing normalised radius values from 0 to 0.5.  Grid is
    *         quadrant shifted so that 0 frequency is at radius(1,1).
    *         u1, u2: Grids containing normalised frequency values
    *         ranging from -0.5 to 0.5 in x and y directions
    *         respectively. u1 and u2 are quadrant shifted.
    */
  def apply(rows: Int, columns: Int): (Matrix, Matrix, Matrix) = {
    require(isPowerOf2(rows), "spectral filters must be power of 2 in size")
    require(isPowerOf2(columns), "spectral filters must be power of 2 in size")

    val u1range =
      indexRange(-columns / 2, columns / 2 - 1) / columns
    val u2range =
      indexRange(-rows / 2, rows / 2 - 1) / rows
    val (u1Raw, u2Raw) = meshgrid(u1range, u2range)

    val u1 = ifftshift(u1Raw)
    val u2 = ifftshift(u2Raw)
    val radius = ((u1 :* u1) + (u2 :* u2)).map(x => math.sqrt(x).toFloat)
    (radius, u1, u2)
  }
}
