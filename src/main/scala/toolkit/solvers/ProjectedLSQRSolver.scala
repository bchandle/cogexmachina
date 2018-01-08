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

package toolkit.solvers

import scala.annotation.tailrec

/**
  * 
  * @author Matthew Pickett
  */
object ProjectedLSQRSolver {
  /** Apply a set of LSQR iterations
  * @param b the result of the linear operation to be inverted
  * @param op the linear operator A in A.x=b
  * @param adj the operator that is adjoint to A
  * @param x0 the initial guess of the solution
  * @param iterations number of iterations to perform*/
  def apply(b:FieldArray, op: FieldArray=>FieldArray,
            adj: FieldArray=>FieldArray,
            x0:FieldArray, iterations:Int,
            projection:ProjectionFunction):FieldArray = {
    solverLoop(b, op, adj, x0, iterations, projection)
  }

  @tailrec private def solverLoop(b:FieldArray, op: FieldArray=>FieldArray,
                          adj: FieldArray=>FieldArray,
                          xCur:FieldArray, iterations:Int,
                          projection:ProjectionFunction):FieldArray = {
    val xNext = projection.f(LSQRSolver(b,op,adj,xCur,1))
    if(iterations == 0) xNext else
      solverLoop(b,op,adj,xNext, iterations-1, projection)
  }
}
