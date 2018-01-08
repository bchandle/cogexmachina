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

/** A filter is a matrix with an origin at some place other than (0, 0).
  *
  * @param minRow Minimum row coordinate in filter.
  * @param maxRow Maximum row coordinate in filter.
  * @param minColumn Minimum column coordinate in filter.
  * @param maxColumn Maximum column coordinate in filter.
  *
  * @author Greg Snider
  */
class Filter(val minRow: Int, val maxRow: Int,
             val minColumn: Int, val maxColumn: Int)
{
  require(minRow <= 0 && maxRow >= 0)
  require(minColumn <= 0 && maxColumn >= 0)
  require(maxRow >= minRow && maxColumn >= minColumn)
  /** Values of filter held in a matrix. */
  private val matrix =
    new Matrix(maxRow - minRow + 1, maxColumn - minColumn + 1)

  /** Read an element in the filter coordinate system.
    *
    * @param row The row of the element.
    * @param column The column of the element.
    * @return The value at location (row, column).
    */
  def apply(row: Int, col: Int): Float =
    matrix(row - minRow, col - minColumn)

  /** Write an element in the filter coordinate system.
    *
    * @param row The row of the element.
    * @param column The column of the element.
    * @param value The value to be written to location (row, column).
    */
  def update(row: Int, col: Int, value: Float) {
    matrix(row - minRow, col - minColumn) = value
  }


  /** Write the filter with the values of a matrix that has the same shape.
    *
    * @param m Matrix used to write filter values.
    */
  def update(m: Matrix) {
    require(m.rows == matrix.rows && m.columns == matrix.columns)
    for (row <- 0 until matrix.rows; col <- 0 until matrix.columns)
      matrix(row, col) = m(row, col)
  }

  /** The row indices, from min to max inclusive. */
  def rowIndices = minRow to maxRow

  /** The column indices, from min to max inclusive. */
  def columnIndices = minColumn to maxColumn

  /** Convert filter to a matrix. */
  def toMatrix: Matrix = matrix
}