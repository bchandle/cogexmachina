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

/** Implements a few Matlab functions to make it easier to translate Matlab
  * code to Cog.
  *
  * @author Greg Snider
  */
trait MatlabFunctions {

  def indexRange(first: Int, last: Int): Vector = indexRange(first, 1, last)

  def indexRange(first: Int, step: Int, last: Int): Vector = {
    require(last > first && step > 0,
      "first: " + first + " step: " + step + " last: " + last)
    var length = 0
    for (i <- first to last by step)
      length += 1
    new Vector(length) {
      var index = first
      for (i <- 0 until length) {
        this(i) = index
        index += step
      }
    }
  }

  def meshgrid(xDomain: Vector, yDomain: Vector): (Matrix, Matrix) = {
    val columns = xDomain.length
    val rows = yDomain.length
    val X = new Matrix(rows, columns) {
      for (row <- 0 until rows; col <- 0 until columns)
        this(row, col) = xDomain(col)
    }
    val Y = new Matrix(rows, columns) {
      for (row <- 0 until rows; col <- 0 until columns)
        this(row, col) = yDomain(row)
    }
    (X, Y)
  }

  def fftshift(m: Matrix): Matrix = {
    val rowOffset = m.rows / 2
    val colOffset = m.columns / 2
    m.shiftCyclic(rowOffset, colOffset)
  }

  def fftshift(v: Vector): Vector = {
    val offset = v.length / 2
    v.shiftCyclic(offset)
  }

  def ifftshift(m: Matrix): Matrix = {
    val rowOffset = m.rows / 2
    val colOffset = m.columns / 2
    m.shiftCyclic(-rowOffset, -colOffset)
  }

  def ifftshift(v: Vector): Vector = {
    val offset = v.length / 2
    v.shiftCyclic(-offset)
  }

  def cart2pol(fx: Matrix, fy: Matrix): (Matrix, Matrix) = {
    require(fx.rows == fy.rows && fx.columns == fy.columns)
    val theta = new Matrix(fx.rows, fx.columns)
    val rho = new Matrix(fx.rows, fx.columns)
    for (row <- 0 until fx.rows; col <- 0 until fx.columns) {
      val x = fx(row, col)
      val y = fy(row, col)
      theta(row, col) = math.atan2(-y, x).toFloat
      rho(row, col) = math.sqrt(x * x + y * y).toFloat
    }
    (theta, rho)
  }

  /** Implement Matlab's repmat.
    *
    * @param matrix Matrix to be replicated.
    * @param rowRep Row replication factor.
    * @param colRep Column replication factor.
    */
  def repmat(matrix: Matrix, rowRep: Int, colRep: Int): Matrix = {
    val result = new Matrix(matrix.rows * rowRep, matrix.columns * colRep)
    for (tileRow <- 0 until rowRep; tileCol <- 0 until colRep) {
      val rowOffset = tileRow * matrix.rows
      val colOffset = tileCol * matrix.columns
      for (row <- 0 until matrix.rows; col <- 0 until matrix.columns)
        result(row + rowOffset, col + colOffset) = matrix(row, col)
    }
    result
  }

  def rot90(m: Matrix) = new Matrix(m.columns, m.rows) {
    for (row <- 0 until rows; col <- 0 until columns)
      this(row, columns - col - 1) = m(m.rows - col - 1, m.columns - row - 1)
  }

  def rot90(m: Matrix, factor: Int): Matrix = {
    require(factor >= 1)
    var rotated = m
    for (i <- 0 until factor)
      rotated = rot90(rotated)
    rotated
  }

  def rot90(v: Vector): Matrix =
    rot90(toMatrix(v))

  def rot90(v: Vector, factor: Int): Matrix =
    rot90(toMatrix(v), factor)

  /** Matlab's flipup operator. */
  def flipud(m: Matrix) = new Matrix(m.rows, m.columns) {
    for (r <- 0 until rows; c <- 0 until columns) {
      this(r, c) = m(rows - r - 1, c)
    }
  }

  /** Matlab's fliplr operator. */
  def fliplr(m: Matrix) = new Matrix(m.rows, m.columns) {
    for (r <- 0 until rows; c <- 0 until columns) {
      this(r, c) = m(r, columns - c - 1)
    }
  }

  /** Matlab's side-by-side matrix concatenation operator. */
  def concat(left: Matrix, right: Matrix): Matrix = {
    require(left.rows == right.rows,
       "left rows: " + left.rows + ", right rows = " + right.rows)
    val result = new Matrix(left.rows, left.columns + right.columns)
    // Copy left
    for (r <- 0 until left.rows; c <- 0 until left.columns)
      result(r, c) = left(r, c)
    // Copy right
    for (r <- 0 until right.rows; c <- 0 until right.columns)
      result(r, c + left.columns) = right(r, c)
    result
  }

  def concat(left: Vector, right: Vector): Matrix =
    concat(toMatrix(left), toMatrix(right))

  /** Matlab's top-on-bottom matrix concatenation operator. */
  def stack(top: Matrix, bottom: Matrix): Matrix = {
    require(top.columns == bottom.columns)
    val result = new Matrix(top.rows + bottom.rows, top.columns)
    // Copy top
    for (r <- 0 until top.rows; c <- 0 until top.columns)
      result(r, c) = top(r, c)
    // Copy bottom
    for (r <- 0 until bottom.rows; c <- 0 until bottom.columns)
      result(r + top.rows, c) = bottom(r, c)
    result
  }

  /** Does a convolution of two matrices.
    *
    * @param a Image to be convolved.
    * @param b Convolution filter.
    * @param shape "full",  "same", or "valid"
    */
  def conv2(a: Matrix, b: Matrix, shape: String = "full"): Matrix = {
    // Have to flip the filter for convolution.
    val bFlipped = flipud(fliplr(b))
    var outRows = a.rows + b.rows - 1
    val outColumns = a.columns + b.columns - 1
    // Expand a to the size of the result, placing it in the middle surrounded
    // by zeros.
    val aExpand = new Matrix(outRows + b.rows + 1, outColumns + b.columns + 1) {
      for (row <- 0 until a.rows; col <- 0 until a.columns)
        this(row + b.rows - 1, col + b.columns - 1) = a(row, col)
    }
    // Now visit every point in the result, doing pointwise convolution.
    val fullOutput = new Matrix(outRows, outColumns) {
      for (row <- 0 until rows; col <- 0 until columns) {
        var value = 0f
        for (bRow <- 0 until b.rows; bCol <- 0 until b.columns) {
          value += bFlipped(bRow, bCol) * aExpand(row + bRow, col + bCol)
        }
        this(row, col) = value
      }
    }
    shape match {
      case "full" =>
        fullOutput
      case "same" =>
        val rowOffset = b.rows / 2
        val colOffset = b.columns / 2
        val output = new Matrix(a.rows, a.columns) {
          for (row <- 0 until rows; col <- 0 until columns)
            this(row, col) = fullOutput(row + rowOffset, col + colOffset)
        }
        output
      case "valid" =>
        val outRows = (a.rows - (0 max (b.rows - 1))) max 0
        val outCols = (a.columns - (0 max (b.columns - 1))) max 0
        val rowOffset = b.rows - 1
        val colOffset = b.columns - 1
        val output = new Matrix(outRows, outCols) {
          for (row <- 0 until rows; col <- 0 until columns)
            this(row, col) = fullOutput(row + rowOffset, col + colOffset)
        }
        output
      case x =>
        throw new Exception("illegal convolution shape: " + shape)
    }
  }


  /** Clip off last column of matrix `m`. */
  def clipLastColumn(m: Matrix) = new Matrix(m.rows, m.columns - 1) {
    for (r <- 0 until rows; c <- 0 until columns)
      this(r, c) = m(r, c)
  }

  /** Clip off last row of matrix `m`. */
  def clipLastRow(m: Matrix) = new Matrix(m.rows - 1, m.columns) {
    for (r <- 0 until rows; c <- 0 until columns)
      this(r, c) = m(r, c)
  }

  /** Lower triangular part of (square) matrix. */
  def tril(m: Matrix): Matrix = {
    require(m.rows == m.columns, "tril only implemented for square matrices")
    new Matrix(m.rows, m.columns) {
      for (r <- 0 until rows; c <- 0 until columns)
        if (r >= c)
          this(r, c) = m(r, c)
    }
  }

  def ones(rows: Int, columns: Int) = new Matrix(rows, columns) {
    for (r <- 0 until rows; c <- 0 until columns)
      this(r, c) = 1
  }

  def polyval(coefficients: Vector, elements: Matrix): Matrix = {
    new Matrix(elements.rows, elements.columns) {
      for (r <- 0 until rows; c <- 0 until columns)
        this(r, c) = polyval(coefficients, elements(r, c))
    }
  }

  def polyval(coefficients: Vector, elements: Vector): Vector = {
    new Vector(elements.length) {
      for (i <- 0 until this.length)
        this(i) = polyval(coefficients, elements(i))
    }
  }

  def polyval(coefficients: Vector, element: Float): Float = {
    val degree = coefficients.length - 1
    def pow(x: Float, exp: Int) = math.pow(x, exp).toFloat
    var sum = 0f
    for (i <- 0 until coefficients.length) {
      val coeff = coefficients(i)
      val exponent = degree - i
      sum += coeff * pow(element, exponent)
    }
    sum
  }


  /** Matlab spec says this produces a "row vector", which is a matrix
    * with a single row in Cog.
    */
  def linspace(low: Float, high: Float, points: Int = 100): Matrix = {
    require(high > low)
    require(points >= 1)
    val result = new Matrix(1, points)
    if (points == 1)
      result(0, 0) = high
    else {
      val spaces = points - 1
      val delta = (high - low) / spaces
      var value = low
      for (i <- 0 until points) {
        result(0, i) = value
        value += delta
      }
    }
    result
  }

  private def toMatrix(vector: Vector) = new Matrix(vector.length, 1) {
    for (row <- 0 until vector.length)
      this(row, 0) = vector(row)
  }
}

/**
 * Test code for MatlabFunctions
 */
object TestMatlabFunctions extends MatlabFunctions {
  def main(args: Array[String]) {
    testMeshgrid()
    testFftshift()
    testCart2pol()
    testRepmat()
    testRot90()
    testConv2()
    testLinspace()
  }

  private def testMeshgrid() {
    val x = indexRange(1, 3)
    val y = indexRange(10, 14)
    val m = meshgrid(x, y)
    val X = m._1
    val Y = m._2
    x.print
    y.print
    X.print
    Y.print
  }

  private def testFftshift() {
    val m = Matrix(
      Array[Float](1, 2, 3, 4, 5),
      Array[Float](6, 7, 8, 9, 10)
    )
    m.print
    val mShifted = fftshift(m)
    mShifted.print
    val mRecovered = ifftshift(mShifted)
    mRecovered.print
  }

  private def testCart2pol() {
    val N = 8
    val f = indexRange(-N/2, N/2 - 1)
    val (fx, fy) = meshgrid(f, f)
    val (theta, rho) = cart2pol(fx, fy)
    println("rho matrix:")
    rho.print
    println("theta matrix:")
    theta.print
  }

  private def testRepmat() {
    val matrix = Matrix(
      Array(1f, 2f, 3f),
      Array(4f, 5f, 6f)
    )
    val repl = repmat(matrix, 3, 2)
    val expected = Matrix(
      Array(1f, 2f, 3f, 1f, 2f, 3f),
      Array(4f, 5f, 6f, 4f, 5f, 6f),
      Array(1f, 2f, 3f, 1f, 2f, 3f),
      Array(4f, 5f, 6f, 4f, 5f, 6f),
      Array(1f, 2f, 3f, 1f, 2f, 3f),
      Array(4f, 5f, 6f, 4f, 5f, 6f)
    )
    require(repl === expected)
  }

  private def testRot90() {
    val x = Matrix(
      Array(1f, 2, 3),
      Array(4f, 5, 6),
      Array(7f, 8, 9),
      Array(0f, 2, 4)
    )
    val rotated = rot90(x)
    x.print
    rotated.print

    val expected = Matrix(
      Array(3f, 6, 9, 4),
      Array(2f, 5, 8, 2),
      Array(1f, 4, 7, 0)
    )
    require(rotated === expected)
  }

  private def testConv2() {
    val image1 = Matrix(
      Array( 1.000000e+00f, 2.000000e+00f, 3.000000e+00f, 4.000000e+00f),
      Array( 5.000000e+00f, 6.000000e+00f, 7.000000e+00f, 8.000000e+00f),
      Array( 9.000000e+00f, 1.000000e+01f, 1.100000e+01f, 1.200000e+01f),
      Array( 1.300000e+01f, 1.400000e+01f, 1.500000e+01f, 1.600000e+01f)
    )
    val filterOdd = Matrix(
      Array( 2.000000e+00f, 5.000000e+00f, 1.000000e+00f),
      Array( -3.000000e+00f, 6.000000e+00f, 9.000000e+00f),
      Array( 1.100000e+01f, 1.300000e+01f, 1.500000e+01f)
    )
    // Computed by Matlab
    val oddFullExpected = Matrix(
      Array( 2.000000e+00f, 9.000000e+00f, 1.700000e+01f, 2.500000e+01f, 2.300000e+01f, 4.000000e+00f),
      Array( 7.000000e+00f, 3.700000e+01f, 6.100000e+01f, 8.100000e+01f, 9.800000e+01f, 4.400000e+01f),
      Array( 1.400000e+01f, 1.120000e+02f, 2.150000e+02f, 2.740000e+02f, 2.790000e+02f, 1.440000e+02f),
      Array( 5.400000e+01f, 2.480000e+02f, 4.510000e+02f, 5.100000e+02f, 4.750000e+02f, 2.440000e+02f),
      Array( 6.000000e+01f, 2.630000e+02f, 5.420000e+02f, 5.930000e+02f, 5.520000e+02f, 3.240000e+02f),
      Array( 1.430000e+02f, 3.230000e+02f, 5.420000e+02f, 5.810000e+02f, 4.330000e+02f, 2.400000e+02f)
    )
    // Computed by Matlab
    val oddSameExpected = Matrix(
      Array( 3.700000e+01f, 6.100000e+01f, 8.100000e+01f, 9.800000e+01f),
      Array( 1.120000e+02f, 2.150000e+02f, 2.740000e+02f, 2.790000e+02f),
      Array( 2.480000e+02f, 4.510000e+02f, 5.100000e+02f, 4.750000e+02f),
      Array( 2.630000e+02f, 5.420000e+02f, 5.930000e+02f, 5.520000e+02f)
    )
    // Computed by Matlab
    val oddValidExpected = Matrix(
      Array( 2.150000e+02f, 2.740000e+02f),
      Array( 4.510000e+02f, 5.100000e+02f)
    )

    require(conv2(image1, filterOdd, "full") ~== oddFullExpected)
    require(conv2(image1, filterOdd, "same") ~== oddSameExpected)

    val filterEven = Matrix(
      Array( 2.000000e+00f, 5.000000e+00f),
      Array( -3.000000e+00f, 6.000000e+00f)
    )
    // Computed by Matlab
    val evenFullExpected = Matrix(
      Array( 2.000000e+00f, 9.000000e+00f, 1.600000e+01f, 2.300000e+01f, 2.000000e+01f),
      Array( 7.000000e+00f, 3.700000e+01f, 4.700000e+01f, 5.700000e+01f, 6.400000e+01f),
      Array( 3.000000e+00f, 7.700000e+01f, 8.700000e+01f, 9.700000e+01f, 1.080000e+02f),
      Array( -1.000000e+00f, 1.170000e+02f, 1.270000e+02f, 1.370000e+02f, 1.520000e+02f),
      Array( -3.900000e+01f, 3.600000e+01f, 3.900000e+01f, 4.200000e+01f, 9.600000e+01f)
    )
    // Computed by Matlab
    val evenSameExpected = Matrix(
      Array( 3.700000e+01f, 4.700000e+01f, 5.700000e+01f, 6.400000e+01f),
      Array( 7.700000e+01f, 8.700000e+01f, 9.700000e+01f, 1.080000e+02f),
      Array( 1.170000e+02f, 1.270000e+02f, 1.370000e+02f, 1.520000e+02f),
      Array( 3.600000e+01f, 3.900000e+01f, 4.200000e+01f, 9.600000e+01f)
    )
    // Computed by Matlab
    val evenValidExpected = Matrix(
      Array( 3.700000e+01f, 4.700000e+01f, 5.700000e+01f),
      Array( 7.700000e+01f, 8.700000e+01f, 9.700000e+01f),
      Array( 1.170000e+02f, 1.270000e+02f, 1.370000e+02f)
    )

    require(conv2(image1, filterEven, "full") ~== evenFullExpected)
    require(conv2(image1, filterEven, "same") ~== evenSameExpected)
  }

  private def testLinspace() {
    val low = 1.0f
    val high = 2.0f
    val Points = 5
    val expected = Matrix(
      Array(1.0f, 1.25f, 1.5f, 1.75f, 2.0f)
    )
    val actual = linspace(low, high, Points)
    require(actual ~== expected)
  }
}
