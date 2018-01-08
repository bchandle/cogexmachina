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

/** Constructs a 2D FIR filter from a 1D filter.
  *
  * Implements mctrans.m
  *
  * @author Greg Snider
  */
object McClellanTransform extends MatlabFunctions {


  /** Construct a 2D filter from a 1D filter.
    *
    * @param b The 1D filter (a row vector, represented as a single row matrix).
    * @param t Transform to use in constructing 2D filter
    * @return The 2D filter.
    *
    */
  def apply(b: Matrix, t: Matrix): Matrix = {
    require(b.rows == 1, "b must be a row vector")

    // Convert b to sum_n a(n) cos(wn) form
    val n = (b.columns - 1) / 2
    val bb: Matrix = rot90(fftshift(rot90(b, 2)), 2)
    val a: Matrix =
      concat(bb(0 to 0, 0 to 0), bb(0 to 0, 1 to n) * 2)
    val insetRows = (t.rows - 1) / 2
    val insetCols = (t.columns - 1) / 2

    // Use Chebyshev polynomials to compute h
    var P0 = new Matrix(1, 1) + 1
    var P1 = t
    var h = P1 * a(0, 1)
    var rows: Range = insetRows until insetRows + 1
    var cols: Range = insetCols until insetCols + 1
    h(rows, cols) = h(rows, cols) + P0 * a(0, 0)

    for (i <- 2 to n) {
      val P2: Matrix = conv2(t, P1) * 2
      rows = new Range(rows.start + insetRows, rows.end + insetRows, rows.step)
      cols = new Range(cols.start + insetCols, cols.end + insetCols, cols.step)
      P2(rows, cols) =
              if (P0.rows == 1 && P0.columns == 1) {
                val result = P2(rows, cols) - P0(0, 0)
                result
              } else {
                P2(rows, cols) - P0
              }
      rows = new Range(insetRows, insetRows + P1.rows, 1)
      cols = new Range(insetCols, insetCols + P1.columns, 1)
      val hh = h
      h = P2 * a(0, i)
      h(rows, cols) = h(rows, cols) + hh
      P0 = P1
      P1 = P2
    }
    h = rot90(h, 2)
    h
  }
}
