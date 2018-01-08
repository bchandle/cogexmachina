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


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

/** Test code.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class SamplingMatricesSpec extends FunSuite with MustMatchers {
  test("all") {
    import SamplingMatrices._

    // Eqns 3.18 must hold
    require(R2 * D1 * R3 == R3 * D2 * R2)
    require(Q1 == R2 * D1 * R3)

    require(R1 * D1 * R4 == R4 * D2 * R1)
    require(Q2 == R1 * D1 * R4)
  }
}