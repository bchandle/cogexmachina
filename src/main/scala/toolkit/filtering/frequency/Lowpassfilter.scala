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

/** Implements Peter Kovesi's lowpassfilter function from his computer vision
  * library: http://www.csse.uwa.edu.au/~pk/research/matlabfns/
  *
  * @author Greg Snider
  */
object Lowpassfilter extends Logarithm with MatlabFunctions {

  /** Generates a lowpass Butterworth filter in the frequency domain
    *
    * @param rows Number of rows in filter.
    * @param columns Number of columns in filter.
    * @param cutoff The cutoff frequency of the filter 0 - 0.5
    * @param n The order of the filter, the higher n is the sharper
    *        the transition is. (n must be an integer >= 1).
    *        Note that n is doubled so that it is always an even integer.
    * @return The Butterworth filter as a matrix.
    */
  def apply(rows: Int, columns: Int, cutoff: Float, n: Int): Matrix = {
    require(isPowerOf2(rows), "spectral filters must be power of 2 in size")
    require(isPowerOf2(columns), "spectral filters must be power of 2 in size")
    require(cutoff > 0 && cutoff < 0.5f, "illegal cutoff frequency")
    require(n >= 1, "filter order must be greater than 0")

    val xrange = if (columns % 2 == 1)
      indexRange(-(columns - 1)/ 2, (columns - 1) / 2) / (columns - 1)
    else
      indexRange(-columns / 2, columns / 2 - 1) / columns

    val yrange = if (rows % 2 == 1)
      indexRange(-(rows - 1) / 2, (rows - 1) / 2) / (rows - 1)
    else
      indexRange(-rows / 2, rows / 2 - 1) / rows

    val mesh = meshgrid(xrange, yrange)
    val x: Matrix = mesh._1
    val y: Matrix = mesh._2
    val radius = ((x :* x) + (y :* y)).map(math.sqrt(_).toFloat)
    val ones = new Matrix(rows, columns) + 1
    val filter = ifftshift(
      ones :/ ((radius / cutoff).map(v => math.pow(v, 2 * n).toFloat) + 1))
    filter
  }
}


object TestLowpassfilter extends App {
  val filter = Lowpassfilter(8, 8, 0.45f, 15)
  filter.print
}