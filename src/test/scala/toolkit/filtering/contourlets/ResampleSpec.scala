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
import toolkit.filtering.contourlets.SamplingMatrices._

/** Test code.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class ResampleSpec extends FunSuite with MustMatchers {
  test("all") {

    val oddFilter = Matrix(
      Array( 1f, 2, 3, -3),
      Array( 4f, 5, 6, -6),
      Array( 7f, 8, 9, -9),
      Array(10f, 11, 12, 13)
    )
    // For sampling matrices R1, R2, R3, R4, resampling is the same as shear.
    require(Resample(oddFilter, R1) === Shear(oddFilter, R1))
    require(Resample(oddFilter, R2) === Shear(oddFilter, R2))
    require(Resample(oddFilter, R3) === Shear(oddFilter, R3))
    require(Resample(oddFilter, R4) === Shear(oddFilter, R4))
  }
}