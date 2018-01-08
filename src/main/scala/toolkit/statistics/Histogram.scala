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


object Histogram {
  /**
   * A function for generating a uniformly binned histogram from the elements of
   * a field. Bins are dynamically positioned according to the input x.
   * @param x the field to count
   * @param bins number of bins to use
   * @return a 0D vector field of length bins with the number of counts per bin
   * @author Matthew Pickett
   */
  def apply(x:Field, bins:Int):Array[Field] = {
    val max = x.reduceMax.fieldReduceMax
    val min = x.reduceMin.fieldReduceMin
    val span = max - min

    val binBounds = Array.tabulate(bins+1){
      (i) => {
        val bound = min + span*(i.toFloat/bins.toFloat)
        //push the final bound up a bit to be inclusive
        if(i==bins) bound*1.00001f else bound
      }
    }

    Array.tabulate(bins){
      (i) => {
        ((x >= binBounds(i))*(x<binBounds(i+1))).reduceSum.fieldReduceSum
      }
    }

  }
}
