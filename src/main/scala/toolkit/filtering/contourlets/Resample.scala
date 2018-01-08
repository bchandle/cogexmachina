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
import SamplingMatrices._

/** Resamples a filter. See "Multidimensional filter banks and multiscale
  * geometric representations," Do and Lu, 2011.
  *
  * @author Greg Snider
  */
object Resample {

  /** Resample a filter. The filter is assumed to have its origin at the
    * center. If the number of rows (or columns) is even and additional zero
    * row (or column) is implicitly added.
    *
    * @param filterMatrix The filter to be resampled.
    * @param sampleMatrix Transform from coordinate system of the input filter
    *        the coordinate system of the output filter. See the paper by Do
    *        and Lu.
    * @return Resampled filter.
    */
  def apply(filterMatrix: Matrix, sampleMatrix: Matrix): Matrix = {
    require(sampleMatrix.rows == 2 && sampleMatrix.columns == 2,
      "invalid 2D sampling matrix, must be 2 x 2")
    val m00 = sampleMatrix(0, 0).toInt
    val m01 = sampleMatrix(0, 1).toInt
    val m10 = sampleMatrix(1, 0).toInt
    val m11 = sampleMatrix(1, 1).toInt
    require(m00 == sampleMatrix(0, 0), "sampling matrix must be integral")
    require(m01 == sampleMatrix(0, 1), "sampling matrix must be integral")
    require(m10 == sampleMatrix(1, 0), "sampling matrix must be integral")
    require(m11 == sampleMatrix(1, 1), "sampling matrix must be integral")

    // We assume that filterMatrix is "centered." This is well-defined for
    // odd length rows and columns. If either rows or columns is even, we
    // round up to obtain the center.
    val filterMinRow = -filterMatrix.rows / 2
    val filterMaxRow = filterMinRow + filterMatrix.rows - 1
    val filterMinColumn = -filterMatrix.columns / 2
    val filterMaxColumn = filterMinColumn + filterMatrix.columns - 1
    val filter = new Filter(filterMinRow, filterMaxRow,
      filterMinColumn, filterMaxColumn)
    filter.update(filterMatrix)

    // Transform from input filter coordinates (`row`, `col`) to output filter
    // coordinates.
    def transform(row: Int, col: Int): (Int, Int) = {
      // Some subtle trickiness here. The sample matrix, by convention, uses
      // a right-handed coordinate system, but (row, col) is a left-handed
      // coordinate system, so we must correct for that here.
      (m00 * row + m01 * -col, m10 * -row + m11 * col)
    }

    // Determine size of the resulting filter given the sampleMatrix. We do
    // this brute force.
    var minRow = 0
    var minColumn = 0
    var maxRow = 0
    var maxColumn = 0
    for (row <- filter.rowIndices; col <- filter.columnIndices) {
      val (outRow, outCol) = transform(row, col)
      minRow = minRow min outRow
      minColumn = minColumn min outCol
      maxRow = maxRow max outRow
      maxColumn = maxColumn max outCol
    }
    val outFilter = new Filter(minRow, maxRow, minColumn, maxColumn)

    // Now map the input filter elements to the output filter.
    for (row <- filter.rowIndices; col <- filter.columnIndices) {
      val (outRow, outCol) = transform(row, col)
      outFilter(outRow, outCol) = filter(row, col)
    }
    outFilter.toMatrix
  }
}
