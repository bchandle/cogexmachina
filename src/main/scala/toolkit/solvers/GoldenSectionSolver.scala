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
import annotation.tailrec

/**
  * Solver for finding the minimum position of nonlinear, convex scalar function f
  * on the line between bound1 and bound2. Implements the algorithm from:
  * https://en.wikipedia.org/wiki/Golden_section_search
  *
  @param bound1 first bound for the solution
  @param bound2 second bound for the solution, must be same type structure as
    bound 1
  @param f the function to optimize over. takes a field array with same type
    structure as bound1 and bound2 and returns a zero-d scalar field
  @param iterations number of iterations
  */
object GoldenSectionSolver{
  private val phi = (1+math.sqrt(5))/2
  private val resphi = 2f-phi.toFloat

  def solveBounds(bound1:FieldArray, bound2:FieldArray,
            f:(FieldArray)=>Field, iterations: Int) = {
    require(bound1 typesEqual bound2, "bounds need to have same type structure")
    require(f(bound1).fieldType == ScalarField(0f).fieldType,
      "f must return a 0D scalar field")
    require(iterations > 0)

    val bInit = bound1 + resphi*(bound2-bound1)
    solverLoop(bound1, bInit, bound2, f, iterations)
  }

  def solve(bound1:FieldArray, bound2:FieldArray,
            f:(FieldArray)=>Field, iterations: Int) = {
    val (a,c) = solveBounds(bound1, bound2, f, iterations)
    (a+c)/2
  }

  //The recursive loop used to iteratively find
  @tailrec private def solverLoop(a:FieldArray,
                                  b:FieldArray,
                                  c:FieldArray,
                                  f: FieldArray =>Field,
                                  cnt: Int):(FieldArray,FieldArray) = {
    val cbDist = (c-b).sq.fieldReduceSum.reduceSum.arrayReduceSum
    val abDist = (a-b).sq.fieldReduceSum.reduceSum.arrayReduceSum

    val cbBigger = cbDist > abDist
    val abBigger = 1f-cbBigger

    val x = b+resphi*(cbBigger*(c-b) + abBigger*(a-b))

    val fbBigger = f(b) > f(x)
    val fxBigger = 1f-fbBigger

    val newA = b*fbBigger*cbBigger + a*fbBigger*abBigger +
            a*fxBigger*cbBigger + x*fxBigger*abBigger
    val newB = x*fbBigger*cbBigger + x*fbBigger*abBigger +
            b*fxBigger*cbBigger + b*fxBigger*abBigger
    val newC = c*fbBigger*cbBigger + b*fbBigger*abBigger +
            x*fxBigger*cbBigger + c*fxBigger*abBigger

    if(cnt == 0) (a, c)
    else solverLoop(newA, newB, newC, f, cnt-1)
  }
}
