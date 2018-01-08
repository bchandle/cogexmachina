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

package toolkit.statistics

import libcog._

/** Accumulates the mean and variance of all field values over time
  * 
  * @author Matthew Pickett
  */
object Moments {
  def apply(x:Field) = {
    val points = x.fieldShape.points*x.tensorShape.points

    val n = ScalarField(0f)
    val mean = ScalarField(0f)
    val M2 = ScalarField(0f)

    val nCur = n + points
    val delta = x - mean
    val meanCur = mean + delta.reduceSum.fieldReduceSum/nCur
    val M2Cur = M2 + delta.sq.reduceSum.fieldReduceSum

    n <== nCur
    mean <== meanCur
    M2 <== M2Cur

    (meanCur, M2Cur/(nCur-1))
  }
}



object ElementwiseMoments {
  def apply(x:VectorField) = {
    val points = x.fieldShape.points

    val n = VectorField(Shape(), x.tensorShape)
    val mean = VectorField(Shape(), x.tensorShape)
    val M2 = VectorField(Shape(), x.tensorShape)

    val nCur = n + points
    val delta = x - mean
    val meanCur = mean + delta.fieldReduceSum/nCur
    val M2Cur = M2 + delta.sq.fieldReduceSum

    n <== nCur
    mean <== meanCur
    M2 <== M2Cur

    (meanCur, M2Cur/(nCur-1))
  }
}

/** Accumulates the mean and variance of all field values over time
  *
  * @author Matthew Pickett
  */
object PointwiseMoments {
  def apply(x:VectorField) = {
    val n = ScalarField(1f)
    val mean = VectorField(x.fieldShape, x.tensorShape)
    val M2 = VectorField(x.fieldShape, x.tensorShape)

    val nCur = n + 1
    val delta = x - mean
    val meanCur = mean + delta/nCur
    val M2Cur = M2 + delta.sq

    n <== nCur
    mean <== meanCur
    M2 <== M2Cur

    (meanCur, M2Cur/(nCur-1))
  }
}

/*
 Algorithm taken from:
     https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
 which gives it as:

def online_variance(data):
  n = 0
  mean = 0
  M2 = 0

  for x in data:
    n = n + 1
    delta = x - mean
    mean = mean + delta/n
    M2 = M2 + delta*(x - mean)

  variance = M2/(n - 1)
  return variance
*/