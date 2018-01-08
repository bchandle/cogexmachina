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

/** Creates a 2D modulated filter from a 1D filter.
  *
  * Implements modulate2d.m
  *
  * @author Greg Snider
  */
object Modulate extends MatlabFunctions {

  /** Modulate a filter along rows. */
  def rows(filter: Matrix): Matrix = {
    val (m1, m2) = modulationVectors(filter)
    val mm1 = new Matrix(m1.length, 1) {
      for (row <- 0 until rows)
        this(row, 0) = m1(row)
    }
    val y = filter :* repmat(mm1, 1, filter.columns)
    y
  }

  /** Modulate a filter along columns. */
  def columns(filter: Matrix): Matrix = {
    val (m1, m2) = modulationVectors(filter)
    val mm2 = new Matrix(1, m2.length) {
      for (col <- 0 until columns)
        this(0, col) = m2(col)
    }
    val y = filter :* repmat(mm2, filter.rows, 1)
    y
  }

  /** Modulate a filter along rows and columns. */
  def rowsAndColumns(filter: Matrix): Matrix = {
    val (m1, m2) = modulationVectors(filter)
    val m = m1 outerProduct m2
    val y = filter :* m
    y
  }

  private def modulationVectors(filter: Matrix): (Vector, Vector) = {
    val rows = filter.rows
    val columns = filter.columns
    val rowOrigin = rows / 2
    val columnOrigin = columns / 2
    val n1 = new Vector(rows) {
      for (col <- 0 until length)
        this(col) = col - rowOrigin
    }
    val n2 = new Vector(columns) {
      for (col <- 0 until length)
        this(col) = col - columnOrigin
    }
    val m1 = n1.map(x => math.pow(-1, x).toInt)
    val m2 = n2.map(x => math.pow(-1, x).toInt)
    (m1, m2)
  }
}
