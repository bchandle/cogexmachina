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

import libcog._
import scala.annotation.tailrec

/**
  * A solver for inverting linear problems of the form A.x = b using the LSQR
  * method outlined in:
  *
  * C. C. Paige and M. A. Saunders, Algorithm 583; LSQR: Sparse linear equations
  *   and least-squares problems, TOMS 8(2), 195-209 (1982).
  *
  * LSQR uses a functional representation of applying the A matrix, op(), in order
  * to optimally perform sparse operations. The algorithm also requires explicit
  * definition of the operator that is adjoint to op(), adj(). operator/adjoint
  * pairs can be debugged with an AdjointTester
  *
  * b and x0 can be Fields or FieldArrays of any type structure, under the
  * following conditions:
  *   1) op(x0) returns a type structure equivalent to b
  *   2) adj(b) returns a type structure equivalent to x0
  *
  * @author Matthew Pickett
  */
object LSQRSolver{
  private def norm(x:FieldArray):ScalarField = x.sq.fieldReduceSum.reduceSum.arrayReduceSum.sqrt

  @tailrec private def LSQRLoop(op: FieldArray => FieldArray,
                                adj: FieldArray => FieldArray,
                                beta:FieldArray, u:FieldArray,
                                alpha:FieldArray, v:FieldArray,
                                w:FieldArray, x:FieldArray,
                                phiBar:FieldArray, rhoBar:FieldArray,
                                i:Int, b:FieldArray): FieldArray ={
    //continue bidiagonalization
    val betauNext = op(v) - alpha*u
    val betaNext = norm(betauNext)
    val uNext = betauNext/betaNext

    val alphavNext = adj(uNext) - betaNext*v
    val alphaNext = norm(alphavNext)
    val vNext = alphavNext/alphaNext

    //construct and apply next orthogonal transformation
    val rho = (rhoBar.sq+betaNext.sq).sqrt
    val c = rhoBar/rho
    val s = betaNext/rho
    val thetaNext = s*alphaNext
    val rhoBarNext = -c*alphaNext
    val phi = c*phiBar
    val phiBarNext = s*phiBar

    //update x,w
    val xNext =  x + (phi/rho)*w
    val wNext = vNext - (thetaNext/rho)*w

    //recursive loop
    if(i==1) xNext else
      LSQRLoop(op, adj, betaNext, uNext, alphaNext, vNext, wNext, xNext,
        phiBarNext, rhoBarNext, i-1, b:FieldArray)

  }

  /** Apply a set of LSQR iterations
    * @param b the result of the linear operation to be inverted
    * @param op the linear operator A in A.x=b
    * @param adj the operator that is adjoint to A
    * @param x0 the initial guess of the solution
    * @param iterations number of iterations to perform*/
  def apply(b:FieldArray, op: FieldArray=>FieldArray,
          adj: FieldArray=>FieldArray,
          x0:FieldArray, iterations:Int) = {
    require(op(x0) typesEqual b, "op(x0) result types not consistent with b types")
    require(adj(b) typesEqual x0, "adj(b) result types not consistent with x0 types")



    /* Iteratively obtain the solution*/

    val r0 = b-op(x0)
    //establish initial values
    val beta = norm(r0)
    val u = r0/beta
    val alphav = adj(u)
    val alpha = norm(alphav)
    val v = alphav/alpha
    val w = v
    val phiBar = beta
    val rhoBar = alpha
    val dx0 = FieldArray.copyShape(x0)

    //  execute loop
    val dx = LSQRLoop(op, adj, beta, u, alpha, v, w, dx0, phiBar, rhoBar,
      iterations, b)

    //don't update unless norm(r0) is greater than 1e-6f * norm(b)
    // this is a decent stopping criterion to prevent divergence for guesses
    // close to the right answer. If there is a violation, NaNs are scrubbed
    // with the dx min 1e20 line.
    val doUpdate = beta > 1e-6f*norm(b)
    x0+(dx min 1e20f)*doUpdate
  }
}