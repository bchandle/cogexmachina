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

/** Operator which resamples an image by shearing it in various ways. This
  * is not periodic, rather it extends the input where necessary with zeroes.
  * This means the output is always twice as big as the input.
  *
  * The implements the functionality of resampz.m
  *
  * @author Greg Snider
  */
object Shear {

  /** Shear a matrix, extending it as necessary.
    *
    * @param in Input matrix.
    * @param shearMatrix Matrix for shearing operation. Must be in
    *        {R1, R2, R3, R4}.
    * @return The resampled / sheared input image.
    */
  def apply(in: Matrix, shearMatrix: Matrix): Matrix = {
    require(in.rows == in.columns, "square input field required")
    val (outRows, outColumns) = shearMatrix match {
      case R1 => (2 * in.rows, in.columns)
      case R2 => (2 * in.rows, in.columns)
      case R3 => (in.rows, 2 * in.columns)
      case R4 => (in.rows, 2 * in.columns)
      case x => throw new RuntimeException("illegal shear matrix")
    }
    var out = new Matrix(outRows, outColumns)

    // Write the iinput, sheared.
    for (inRow <- 0 until in.rows; inCol <- 0 until in.columns) {
      val pixel = in(inRow, inCol)
      val outRow: Int = shearMatrix match {
        case R1 => inRow + in.rows - inCol
        case R2 => inRow + inCol
        case R3 => inRow
        case R4 => inRow
        case x => throw new RuntimeException("illegal shear matrix")
      }
      val outCol: Int = shearMatrix match {
        case R1 => inCol
        case R2 => inCol
        case R3 => inCol + in.columns - inRow
        case R4 => inCol + inRow
        case x => throw new RuntimeException("illegal shear matrix")
      }
      out(outRow, outCol) =  pixel
    }
    out
    if (shearMatrix == R1 || shearMatrix == R2) {
      // For R1 and R2, we need to remove leading and trailing zero rows
      var start = 0
      var end = out.rows - 1
      while (out.row(start).abs.reduce(_ + _) == 0)
        start += 1
      while (out.row(end).abs.reduce(_ + _) == 0)
        end -= 1
      val rowRange = start to end
      val colRange = 0 until out.columns
      out.submatrix(rowRange, colRange)
    } else {
      // For R3 and R4, we need to remove leading and trailing zero columns
      var start = 0
      var end = out.columns - 1
      while (out.column(start).abs.reduce(_ + _) == 0)
        start += 1
      while (out.column(end).abs.reduce(_ + _) == 0)
        end -= 1
      val colRange = start to end
      val rowRange = 0 until out.rows
      out.submatrix(rowRange, colRange)
    }
  }
}

/** An instance of the Shear operator.
  *
  * Used only by the companion object.
  *
  * @param shearType Type of shearing to perform.
  */
class Shear private (shearMatrix: Matrix)
        extends Operator
{
  /** Shear an image.
    *
    * @param in Input image.
    * @param out Output subfield.
    */
  def compute(in: ScalarFieldReader, out: ScalarFieldWriter) {
    require(in.fieldShape.dimensions == 2, "illegal input for shear")
    require(in.tensorShape.dimensions == 0, "illegal input for shear")
    val inRows = in.fieldShape(0)
    val inColumns = in.fieldShape(1)
    require(inRows == inColumns, "square input field required")
    val (outRows, outColumns) = shearMatrix match {
      case R1 => (2 * inRows, inColumns)
      case R2 => (2 * inRows, inColumns)
      case R3 => (inRows, 2 * inColumns)
      case R4 => (inRows, 2 * inColumns)
      case x => throw new RuntimeException("illegal shear matrix")
    }
    val outShape = Shape(outRows, outColumns)
    out.setShape(outShape)

    // Zero the output.
    for (outRow <- 0 until outRows; outCol <- 0 until outColumns) {
      out.write(outRow, outCol, 0f)
    }
    // Write the image, sheared.
    for (inRow <- 0 until inRows; inCol <- 0 until inColumns) {
      val pixel = in.read(inRow, inCol)
      val outRow: Int = shearMatrix match {
        case R1 => inRow + inRows - inCol
        case R2 => inRow + inCol
        case R3 => inRow
        case R4 => inRow
        case x => throw new RuntimeException("illegal shear matrix")
      }
      val outCol: Int = shearMatrix match {
        case R1 => inCol
        case R2 => inCol
        case R3 => inCol + inColumns - inRow
        case R4 => inCol + inRow
        case x => throw new RuntimeException("illegal shear matrix")
      }
      out.write(outRow, outCol, pixel)
    }
  }
}
