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


/*

/** Solves the Lasso problem:
  *         min. ||A.x - b||_2^2 + lambda *||x||_1
  * using an alternating direction method of multipliers approach with
  * an LSQR solver to step the least squares part and the shrinkage function
  * to solve the regularization part.
  *
  * @author: Matthew Pickett
  */
object LSQRLassoSolver{

  @tailrec private def loop(lambdaStyle:L1RegStyle, b:FieldArray,
                            op: FieldArray=>FieldArray,
                            adj: FieldArray=>FieldArray,
                            xCur:FieldArray, iterations:Int):FieldArray=
    if(iterations > 0) {
      val solun =  LSQRSolver(b, op, adj, xCur, 1)
      val xNext:FieldArray = lambdaStyle match {
        case ls:DynamicShrink => {
          val lambda = ls.multiplier*solun.abs.reduceMax.fieldReduceMax.arrayReduceMax
          shrink(solun, lambda)}
        case ls:DynamicShrinkRectify => {
          val clampNeg = solun max 0f
          val lambda = ls.multiplier*clampNeg.reduceMax.fieldReduceMax.arrayReduceMax
          shrink(clampNeg, lambda)
        }
        case ls:StaticShrink => {
          val lambda = ScalarField(ls.lambda)
          shrink(solun, lambda)}
        case ls:DynamicThres => {
          val lambda = ls.multiplier*solun.abs.reduceMax.fieldReduceMax.arrayReduceMax
          threshold(solun, lambda)
        }
      }
      loop(lambdaStyle, b,op, adj, xNext, iterations-1)
    }
    else xCur


  /** Apply a set of LSQR + shrinkage iterations
    * @param lambdaStyle the regularization style
    * @param b the result of the linear operation to be inverted
    * @param op the linear operator A in A.x=b
    * @param adj the operator that is adjoint to A
    * @param x0 the initial guess of the solution
    * @param iterations number of iterations to perform
    */
  def apply(lambdaStyle:L1RegStyle, b:FieldArray,
            op: FieldArray=>FieldArray,
            adj: FieldArray=>FieldArray,
            x0:FieldArray, iterations:Int):FieldArray  =
    loop(lambdaStyle, b,op, adj, x0, iterations)
}
*/

