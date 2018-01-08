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

/** A model for a linear systems
  * @param A The linear system update matrix
  * @param H The measurement model
  * @author Matthew Pickett
  */
class LinearSystemModel(val A:Matrix, val H:Matrix){
  val stateVars = A.rows
  val measVars = H.rows

  require(A.rows == A.columns, "The matrix A must be square")
  require(H.columns == stateVars, "The matrix H must have as many columns as A has rows")
}


/** A simple 2D motion model in which the state is:
  *   x = {rowPos, colPos, rowVel, colVel}
  * The state evolves as pos_(t+1) = pos_t + dt * vel_t
  * The position is directly measured by a single sensor
  * @author Matthew Pickett
  */
object SimpleMotion2D {
  //create model. dt param is the magnitude of the time step
  def apply(dt:Float = 1f) = {
    val Amat = Matrix(Array(
      Array(1f, 0f, dt, 0f),
      Array(0f, 1f, 0f, dt),
      Array(0f, 0f, 1f, 0f),
      Array(0f, 0f, 0f, 1f)
    ))
    val Hmat = Matrix(Array(
      Array(1f, 0f, 0f, 0f),
      Array(0f, 1f, 0f, 0f)
    ))

    new LinearSystemModel(Amat, Hmat)
  }


}
