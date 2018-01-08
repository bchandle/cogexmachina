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
  * 
  * @author Matthew Pickett
  */
object Kmeans {
  def apply(input:VectorField, initialMeans:FieldArray,
            iterations:Int, relevance:ScalarField):FieldArray = {
    loop(input, initialMeans, iterations, relevance)
  }
  def apply(input:VectorField, initialMeans:FieldArray,
            iterations:Int):FieldArray = {
    apply(input, initialMeans, iterations,
      ScalarField(input.fieldShape)+1f)
  }

  private def single(input:VectorField, curMeans:FieldArray, relevance:ScalarField):FieldArray = {
    val clusters = curMeans.length
    val distance = (input-curMeans).sq.reduceSum
    val winners  = distance.toVectorField <= distance.toVectorField.reduceMin
    val winnersMask = winners*relevance
    val curWins = winnersMask.fieldReduceSum + 1f

    val update = FieldArray.tabulate(clusters){
      (i) => (winnersMask.vectorElement(i) * input).fieldReduceSum / curWins.vectorElement(i)
    }
    (curMeans + update)/2
  }

  @tailrec private def loop(input:VectorField, curMeans:FieldArray, i:Int, relevance:ScalarField):FieldArray = {
    if (i==0) curMeans
    else loop(input, single(input, curMeans, relevance), i-1, relevance)
  }
}