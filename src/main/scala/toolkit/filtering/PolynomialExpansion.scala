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
package toolkit.filtering

import libcog._
import toolkit.filtering.spatial.PolynomialFilters


/** Implements a polynomial filter bank.
  *
  * The "input" scalar field is projected
  * locally onto a 2nd-order polynomial approximation, represented by a matrix
  * field + vector field + scalar field. The basis set consists of the
  * following 2-dimensional functions:  1, x, y, x*x, x*y, y*y.
  *
  * Note that this filter bank introduces no delay.
  *
  * This is from Gunnar Farneback's dissertation.
  */
@deprecated("use libcog.filters.spatial.QuadraticExpansion.apply() instead")
class PolynomialExpansion(input: ScalarField, applicability: Matrix) {
  require(input.fieldShape.dimensions == 2)
  private val poly = new PolynomialFilters(applicability)

  /** Cog 4.0 switched the standard kernel representation from a Matrix to a
    * ScalarField. Old code still uses the Matrix standard. This implicit is
    * here to simplify porting for difficult cases.
    *
    * -Ben
    */
  implicit def matrixToScalarField(in: Matrix): ScalarField = {
    ScalarField(in.rows, in.columns, in(_, _))
  }

  // The output coefficients for the projection of "input" onto the basis set.
  // These use an (x, y) coordinate system to be consistent with Farneback's
  // dissertation, but we must convert these to (row, column) coordinates
  // for use within Cog.
  private val one = input.convolve(poly.dcKernel, BorderClamp)
  private val x = input.convolve(poly.xKernel, BorderClamp)
  private val y = input.convolve(poly.yKernel, BorderClamp)
  private val xx: ScalarField = input.convolve(poly.xxKernel, BorderClamp)
  private val xy: ScalarField = input.convolve(poly.xyKernel * 0.5f, BorderClamp)
  private val yy: ScalarField = input.convolve(poly.yyKernel, BorderClamp)

  // The expansion: A, b, c
  val A = matrixField(Array(Array(yy, xy), Array(xy, xx)))
  val b = vectorField(y, x)
  val c = one
}

/** Local polynomial expansion of a scalar field.
  */
@deprecated("use libcog.filters.spatial.QuadraticExpansion.apply() instead")
object PolynomialExpansion {

  /** Project a scalar field locally onto a 2nd-order polynomial approximation,
    * represented by a matrix field + vector field + scalar field. The basis
    * set consists of the following 2-dimensional functions:
    * {{{
    *   1, x, y, x*x, x*y, y*y.
    * }}}
    * This is based on Gunnar Farneback's dissertation.
    *
    * @param input Scalar field to be projected onto polynomial expansion.
    * @return (matrix field, vector field) of the expansion.
    */
  def apply(input: ScalarField,
            applicability: Matrix): (MatrixField, VectorField) =
  {
    val poly = new PolynomialFilters(applicability)
    implicit def matrixToScalarField(in: Matrix): ScalarField = {
      ScalarField(in.rows, in.columns, in(_, _))
    }

    // The output coefficients for the projection of "input" onto the basis set.
    // These use an (x, y) coordinate system to be consistent with Farneback's
    // dissertation, but we must convert these to (row, column) coordinates
    // for use within Cog.
    val x = input.convolve(poly.xKernel, BorderClamp)
    val y = input.convolve(poly.yKernel, BorderClamp)
    val xx: ScalarField = input.convolve(poly.xxKernel, BorderClamp)
    val xy: ScalarField = input.convolve(poly.xyKernel * 0.5f, BorderClamp)
    val yy: ScalarField = input.convolve(poly.yyKernel, BorderClamp)

    // The expansion: A, b, c
    val A = matrixField(Array(Array(yy, xy), Array(xy, xx)))
    val b = vectorField(y, x)
    (A, b)
  }
}