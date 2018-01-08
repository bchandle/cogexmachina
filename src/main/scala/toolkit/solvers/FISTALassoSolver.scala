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



/** Solves the Lasso problem:
  *         min. ||A.x - b||_2^2 + lambda *||x||_1
  * using a FISTA solver. This is the most common optimization problem solved by
  * the FISTA method.
  * 
  * @author: Matthew Pickett
  */
/*
object FISTALassoSolver {
  /** Apply a set of LSQR + shrinkage iterations
    * @param lambdaStyle the regularization style
    * @param b the result of the linear operation to be inverted
    * @param op the linear operator A in A.x=b
    * @param adj the operator that is adjoint to A
    * @param x0 the initial guess of the solution
    * @param iterations number of iterations to perform
    * @param stepSize step size parameter
    */
  def apply(lambdaStyle:L1RegStyle, b:FieldArray,
    op: FieldArray=>FieldArray,
    adj: FieldArray=>FieldArray,
    x0:FieldArray, iterations:Int, stepSize:Float):FieldArray  = {

    val prox = (x:FieldArray) =>
      lambdaStyle match {
        case ls:DynamicShrink =>{
          val lambda = ls.multiplier*x.abs.reduceMax.fieldReduceMax.arrayReduceMax
          shrink(x, lambda)}
        case ls:DynamicShrinkRectify => {
 //         val lambda = ls.multiplier*x.abs.reduceMax.fieldReduceMax.arrayReduceMax
 //         shrink(x, lambda) max 0f
          throw new RuntimeException("not yet implemented")
        }
        case ls:StaticShrink =>{
          val lambda = ScalarField(ls.lambda)
          shrink(x, lambda)}
        case ls:DynamicThres =>{
          val lambda = ls.multiplier*x.abs.reduceMax.fieldReduceMax.arrayReduceMax
          threshold(x, lambda)
        }


    }
    val grad = (x:FieldArray) => adj(op(x) - b)

    FISTASolver(grad, prox, x0, stepSize, iterations)
  }


}
*/