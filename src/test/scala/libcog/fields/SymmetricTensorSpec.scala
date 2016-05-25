/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package libcog.fields

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import cogx._

/** Test code.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class SymmetricTensorSpec extends FunSuite{

  test("all"){
    val random = new Random
    def stickness() = random.nextFloat + 0.1f
    def ballness() = (random.nextFloat + 0.1f)
    def orientation() = math.min(1f, random.nextFloat + 0.00001f) * (math.Pi.toFloat - 0.00001f)
    val Trials = 1000
    for (trial <- 0 until Trials) {
      val s = stickness()
      val b = ballness() min s
      val o = orientation()
      val tensor = new SymmetricTensor(s, b, o)
      require(tensor.stickness ~== s,
        f"stickness mismatch for (s,b,o)=($s,$b,$o) tensor.stickness = ${tensor.stickness}")
      require(tensor.ballness ~== b,
        f"vallness mismatch for (s,b,o)=($s,$b,$o) tensor.ballness = ${tensor.ballness}")
      require(tensor.orientation ~== o,
        f"orientation mismatch for (s,b,o)=($s,$b,$o) tensor.orientation = ${tensor.orientation}")
    }
  }
}