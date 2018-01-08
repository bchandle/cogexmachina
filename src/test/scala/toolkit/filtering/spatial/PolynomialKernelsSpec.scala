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
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Test code for PolynomialKernels. This uses the example from section 3.4 of
 * Farneback's thesis.
 */
@RunWith(classOf[JUnitRunner])
class PolynomialKernelsSpec extends FunSuite {
  test("PolynomialKernels") {
    val image = Matrix(
      Array[Float](1, 1, 2),
      Array[Float](6, 2, 3),
      Array[Float](3, 2, 6)
    )
    val applicability = Matrix(
      Array(1f, 2f, 1f),
      Array(2f, 4f, 2f),
      Array(1f, 2f, 1f)
    )
    val certainty = Matrix(
      Array[Float](2, 2, 2),
      Array[Float](0, 2, 2),
      Array[Float](1, 2, 1)
    )
    val expectedComponents = new Vector(1.8115939f, 0.71739125f,
      0.8623188f, 0.8478261f, 0.40579695f, -0.12318838f)

    val poly = new PolynomialFilters(applicability, certainty)
    val kernels = Array(poly.dcKernel, poly.xKernel, poly.yKernel,
      poly.xxKernel, poly.xyKernel, poly.yyKernel)
    val components = new Vector(6) {
      for (i <- 0 until length)
      // Because the kernels are convolution kernels, we must flip them
      // to do unnormalized correlation.
        this(i) = (kernels(i).flip :* image).reduce(_ + _)
    }
    require(components ~== expectedComponents)
  }
}