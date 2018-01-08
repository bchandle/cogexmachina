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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

/** Test code.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class ParallelogramFiltersSpec extends FunSuite with MustMatchers {
  test("all") {
    val f1 = Matrix(
      Array( 1f,  2f,  3f,  4f),
      Array( 4f,  5f,  6f,  7f),
      Array( 7f,  8f,  9f, 10f),
      Array(10f, 11f, 12f, 13f)
    )

    val f2 = Matrix(
      Array( 7f,  8f,  9f, 10f),
      Array( 1f,  2f,  3f,  4f),
      Array(10f, 11f, 12f, 13f),
      Array( 4f,  5f,  6f,  7f)
    )

    val b1Matlab = Array(
      Matrix(
        Array(0f, 0f, 0f, 4f),
        Array(0f, 0f, 3f, -7f),
        Array(0f, 2f, -6f, 10f),
        Array(1f, -5f, 9f, -13f),
        Array(-4f, 8f, -12f,  0f),
        Array(7f, -11f, 0f, 0f),
        Array(-10f, 0f, 0f, 0f)
      ),

      Matrix(
        Array(1f, 0f, 0f, 0f),
        Array(4f,    -2f,     0f,     0f),
        Array(7f,    -5f,     3f,     0f),
        Array(10f,    -8f,     6f,    -4f),
        Array(0f,   -11f,     9f,    -7f),
        Array(0f,     0f,    12f,   -10f),
        Array(0f,     0f,     0f,   -13f)
      ),

      Matrix(
        Array(0f,    0f,     0f,     1f,    -4f,     7f,   -10f),
        Array(0f,     0f,     2f,    -5f,     8f,   -11f,     0f),
        Array(0f,     3f,    -6f,     9f,   -12f,     0f,     0f),
        Array(4f,    -7f,    10f,   -13f,     0f,     0f,     0f)
      ),

      Matrix(
        Array(1f,     4f,     7f,    10f,     0f,     0f,     0f),
        Array(0f,    -2f,    -5f,    -8f,   -11f,     0f,     0f),
        Array(0f,     0f,     3f,     6f,     9f,    12f,     0f),
        Array(0f,     0f,     0f,    -4f,    -7f,   -10f,   -13f)
      )
    )

    val b2Matlab = Array(
      Matrix(
        Array(0f,     0f,     0f,    10f),
        Array(0f,     0f,     9f,    -4f),
        Array(0f,     8f,    -3f,    13f),
        Array(7f,    -2f,    12f,    -7f),
        Array(-1f,   11f,    -6f,     0f),
        Array(10f,    -5f,     0f,     0f),
        Array(-4f,     0f,     0f,     0f)
      ),

      Matrix(
        Array(7f,     0f,     0f,     0f),
        Array(1f,    -8f,     0f,     0f),
        Array(10f,    -2f,     9f,     0f),
        Array(4f,   -11f,     3f,   -10f),
        Array(0f,    -5f,    12f,    -4f),
        Array(0f,     0f,     6f,   -13f),
        Array(0f,     0f,     0f,    -7f)
      ),

      Matrix(
        Array(0f,     0f,     0f,     7f,    -1f,    10f,    -4f),
        Array(0f,     0f,     8f,    -2f,    11f,    -5f,     0f),
        Array(0f,     9f,    -3f,    12f,    -6f,     0f,     0f),
        Array(10f,    -4f,    13f,    -7f,     0f,     0f,     0f)
      ),

      Matrix(
        Array(7f,     1f,    10f,     4f,     0f,     0f,    0f),
        Array(0f,    -8f,    -2f,   -11f,    -5f,     0f,     0f),
        Array(0f,     0f,     9f,     3f,    12f,     6f,     0f),
        Array(0f,     0f,     0f,   -10f,    -4f,   -13f,    -7f)
      )
    )

    // Compare with output of matlab code.
    val b1 = ParallelogramFilters(f1)
    val b2 = ParallelogramFilters(f2)
    for (i <- 0 until 3) {
      require(b1(i) === b1Matlab(i))
      require(b2(i) === b2Matlab(i))
    }
  }
}