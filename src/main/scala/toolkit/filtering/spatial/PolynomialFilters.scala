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

/**
 * Implements a weighted, polynomial expansion locally at each point in a 2D
 * scalar field. The weighting, specified by the "applicability" matrix, would
 * typically be Gaussian for many applications, but that is not required. The
 * basis set consists of the matrices (1, x, y, x^2, x * y, y^2). To use, simply
 * convolve each of the generated kernels over the field. Each convolution
 * will produce the coefficent for the corresponding basis element.
 *
 * The outputs are the six kernels: "dcKernel", "xKernel", "yKernel",
 * "xxKernel", "xyKernel" and "yyKernel"
 *
 * See "Spatial domain methods for orientation and velocity estimation" by
 * Gunnar Farneback, chapter 3, for a detailed description.
 *
 * @author Greg Snider
 */
@deprecated("Use QuadraticBasis instead")
class PolynomialFilters(applicability: Matrix, certainty: Matrix = null) {
  /** Number of kernels in the basis set. */
//  private val BasisSetSize = 6

  private val rows = applicability.rows
  private val columns = applicability.columns
  require(rows == columns && rows % 2 == 1,
    "Applicability matrix must be square and of odd size.")

  // Compute the basis set
  val dc = new Matrix(rows, columns)
  val x = new Matrix(rows, columns)
  val y = new Matrix(rows, columns)
  for (row <- 0 until rows; col <- 0 until columns) {
    val offset = rows / 2
    val delta = 1.0f / offset
    dc(row, col) = 1f
    x(row, col) = -1 + col * delta
    y(row, col) = -1 + row * delta
  }
  val xx = x :* x
  val xy = x :* y
  val yy = y :* y

  // Create the basis matrix, B
  private val B = Matrix(
    dc.transpose.toVector.toArray,
    x.transpose.toVector.toArray,
    y.transpose.toVector.toArray,
    xx.transpose.toVector.toArray,
    xy.transpose.toVector.toArray,
    yy.transpose.toVector.toArray
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
  // the output kernels.
  val dcKernel = kernelMatrix.row(0).toVector.shape(rows, columns).transpose.flip
  val xKernel = kernelMatrix.row(1).toVector.shape(rows, columns).transpose.flip
  val yKernel = kernelMatrix.row(2).toVector.shape(rows, columns).transpose.flip
  val xxKernel = kernelMatrix.row(3).toVector.shape(rows, columns).transpose.flip
  val xyKernel = kernelMatrix.row(4).toVector.shape(rows, columns).transpose.flip
  val yyKernel = kernelMatrix.row(5).toVector.shape(rows, columns).transpose.flip
}
