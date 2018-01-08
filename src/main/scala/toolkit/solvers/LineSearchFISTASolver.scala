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
  * A solver for finding the minimum of a separable convex function f
  * which is minimized with a gradient step followed by a proximal map. The
  * method is outlined in:
  *
  *   Beck, A. and M. Teboulle (2009). "A Fast Iterative Shrinkage-Thresholding
  *     Algorithm for Linear Inverse Problems." SIAM J. Img. Sci. 2(1): 183-202.
  *
  * @param grad the differentiable portion of the loss function f
  * @param projection proximal map for dealing with the non-differentiable portion
  * @param x0 the initial guess
  * @param stepSize parameter for scaling step distance
  * @param iterations number of iterations to perform
  * @author Matthew Pickett
  */

class LineSearchFISTASolver(grad: (FieldArray)=>FieldArray,
                            projection:ProjectionFunction,
                            x0: FieldArray,
                            stepSize: Float,
                            iterations:Int) {
  def apply(grad: (FieldArray)=>FieldArray,
            projection:ProjectionFunction,
            x0: FieldArray,
            stepSize: Float,
            iterations:Int) = {
    require(grad(x0) typesEqual projection.f(x0),
      "grad and prox operators inconsistent")
    solverLoop(grad, projection, stepSize, iterations, x0, x0, iterations)
  }

  @tailrec private def solverLoop (grad: (FieldArray)=>FieldArray,
                                   projection:ProjectionFunction,
                                   stepSize:Float,
                                   iterations:Int,
                                   xcur: FieldArray,
                                   xprev:FieldArray,
                                   count: Int):FieldArray = {
    val k = iterations - count
    val xext = xcur + ((xcur-xprev)*((k-2f)/(k+1f)))
    val xnext = projection.f(xext-grad(xext)*stepSize)

    if (count-1 == 0) xnext
    else solverLoop(grad, projection, stepSize, iterations, xnext, xcur, count-1)
  }
}
