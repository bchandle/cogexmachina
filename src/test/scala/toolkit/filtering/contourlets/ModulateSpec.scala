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
class ModulateSpec extends FunSuite with MustMatchers {
  test("all") {
    val oddMatrix = Matrix(
      Array(1f, 2f, 3f),
      Array(4f, 5f, 6f),
      Array(7f, 8f, 9f)
    )
    val evenMatrix = Matrix(
      Array( 1f,  2f,  3f,  4f),
      Array( 4f,  5f,  6f,  7f),
      Array( 7f,  8f,  9f, 10f),
      Array(10f, 11f, 12f, 13f)
    )

    // Output from Matlab modulate2
    val rowsOdd = Matrix(
      Array( -1.000000e+00f, -2.000000e+00f, -3.000000e+00f),
      Array( 4.000000e+00f, 5.000000e+00f, 6.000000e+00f),
      Array( -7.000000e+00f, -8.000000e+00f, -9.000000e+00f)
    )
    val columnsOdd = Matrix(
      Array( -1.000000e+00f, 2.000000e+00f, -3.000000e+00f),
      Array( -4.000000e+00f, 5.000000e+00f, -6.000000e+00f),
      Array( -7.000000e+00f, 8.000000e+00f, -9.000000e+00f)
    )
    val bothOdd = Matrix(
      Array( 1.000000e+00f, -2.000000e+00f, 3.000000e+00f),
      Array( -4.000000e+00f, 5.000000e+00f, -6.000000e+00f),
      Array( 7.000000e+00f, -8.000000e+00f, 9.000000e+00f)
    )
    val rowsEven = Matrix(
      Array( 1.000000e+00f, 2.000000e+00f, 3.000000e+00f, 4.000000e+00f),
      Array( -4.000000e+00f, -5.000000e+00f, -6.000000e+00f, -7.000000e+00f),
      Array( 7.000000e+00f, 8.000000e+00f, 9.000000e+00f, 1.000000e+01f),
      Array( -1.000000e+01f, -1.100000e+01f, -1.200000e+01f, -1.300000e+01f)
    )
    val columnsEven = Matrix(
      Array( 1.000000e+00f, -2.000000e+00f, 3.000000e+00f, -4.000000e+00f),
      Array( 4.000000e+00f, -5.000000e+00f, 6.000000e+00f, -7.000000e+00f),
      Array( 7.000000e+00f, -8.000000e+00f, 9.000000e+00f, -1.000000e+01f),
      Array( 1.000000e+01f, -1.100000e+01f, 1.200000e+01f, -1.300000e+01f)
    )
    val bothEven = Matrix(
      Array( 1.000000e+00f, -2.000000e+00f, 3.000000e+00f, -4.000000e+00f),
      Array( -4.000000e+00f, 5.000000e+00f, -6.000000e+00f, 7.000000e+00f),
      Array( 7.000000e+00f, -8.000000e+00f, 9.000000e+00f, -1.000000e+01f),
      Array( -1.000000e+01f, 1.100000e+01f, -1.200000e+01f, 1.300000e+01f)
    )

    // Compare with results from modulate2.m
    require(Modulate.rows(oddMatrix) ~== rowsOdd)
    require(Modulate.columns(oddMatrix) ~== columnsOdd)
    require(Modulate.rowsAndColumns(oddMatrix) ~== bothOdd)

    require(Modulate.rows(evenMatrix) ~== rowsEven)
    require(Modulate.columns(evenMatrix) ~== columnsEven)
    require(Modulate.rowsAndColumns(evenMatrix) ~== bothEven)
  }
}