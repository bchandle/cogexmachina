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
package toolkit.dynamicalsystems

import libcog._
import cogio._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
  * 
  * @author: Matthew Pickett
  */

@RunWith(classOf[JUnitRunner])
class PseudoInvertSpec extends FunSuite  {
  test("0D matrix field pseudoinversion") {
    val app = new ComputeGraph {
      val mat = MatrixField(Matrix(Array(
        Array(1f,2f,3f),
        Array(4f,5f,6f),
        Array(7f,8f,9f)
      )))

      val pseudo = PseudoInvert(mat)

      val answer = MatrixField(Matrix(Array(
        Array(-0.638889f, -0.166667f, 0.305556f),
        Array(-0.0555556f, 0.0f, 0.0555556f),
        Array(0.527778f, 0.166667f, -0.194444f)
      )))

      val diff = abs(pseudo-answer)
      val cumulativeError = reduceSum(diff) / diff.tensorShape.points

      probe(cumulativeError)
    }

    app.step
    val error = FieldState.read(app.read(app.cumulativeError)).data(0)
    println(error)
    require(error < 1e-6f)
    app.release
  }
}
