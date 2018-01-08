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

/** Basis filters and their duals for a local, weighted, quadratic
  * expansion locally at each point in a 2D scalar field.
  *
  * See "Spatial domain methods for orientation and velocity estimation" by
  * Gunnar Farneback, chapter 3, for a detailed description.
  *
  * @author Greg Snider
  */
class QuadraticBasis(applicability: Matrix, certainty: Matrix = null) {
  /** Number of kernels in the basis set. */
  //private val BasisSetSize = 6

  private val rows = applicability.rows
  private val columns = applicability.columns
  require(rows == columns && rows % 2 == 1,
    "Applicability matrix must be square and of odd size.")

  // Compute the basis set (synthesis filters). These should use convolution
  // to reconstruct the input. The B prefix stands for "basis". The suffix
  // stands for the direction of differentiation.
  val Bdc = new Matrix(rows, columns)
  val Bcol = new Matrix(rows, columns)
  val Brow = new Matrix(rows, columns)
  for (row <- 0 until rows; col <- 0 until columns) {
    val offset = rows / 2
    Bdc(row, col) = 1f
    Bcol(row, col) = col - offset
    Brow(row, col) = row - offset
  }
  val Bcolcol = Bcol :* Bcol
  val Browcol = Bcol :* Brow
  val Browrow = Brow :* Brow

  // Create the basis matrix, B
  private val B = Matrix(
    Bdc.transpose.toVector.toArray,
    Bcol.transpose.toVector.toArray,
    Brow.transpose.toVector.toArray,
    Bcolcol.transpose.toVector.toArray,
    Browcol.transpose.toVector.toArray,
    Browrow.transpose.toVector.toArray
  ).transpose

  // Create the diagonal version of the applicability matrix, called Wa in
  // Farneback's thesis.
  private val Wa = Matrix.diagonal(applicability.transpose.toVector)

  // The diagonal version of the certainty matrix.
  private val Wc =
    if (certainty != null)
      Matrix.diagonal(certainty.transpose.toVector)
    else
      Matrix.identity(rows * rows)

  // Equation 3.7
  private val dual = (B.transpose * Wa * Wc * B).invert
  private val kernelMatrix = dual * B.transpose * Wa * Wc

  // Untangle the convolution kernels from the matrix notation to produce
  // the dual basis (analysis operators). These should be *cross-correlated^
  // with an input to create the polynomial expansion. The G prefix follows
  // Farneback's convention of naming for the dual basis.
  val Gdc = kernelMatrix.row(0).toVector.shape(rows, columns).transpose
  val Gcol = kernelMatrix.row(1).toVector.shape(rows, columns).transpose
  val Grow= kernelMatrix.row(2).toVector.shape(rows, columns).transpose
  val Gcolcol = kernelMatrix.row(3).toVector.shape(rows, columns).transpose
  val Growcol = kernelMatrix.row(4).toVector.shape(rows, columns).transpose
  val Growrow = kernelMatrix.row(5).toVector.shape(rows, columns).transpose
}
