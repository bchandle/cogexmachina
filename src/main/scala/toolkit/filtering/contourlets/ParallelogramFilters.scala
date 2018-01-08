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

package toolkit.filtering.contourlets

import libcog._
import SamplingMatrices._

/** Generates a set of 4 parallelogram filters from a diamond filter.
  *
  * Implements parafilters.m
  *
  * @author Greg Snider
  */
object ParallelogramFilters {

  /** Generates a group of parallelogram filters from a diamond filter.
    *
    * @param f1 The diamond filter.
    * @return An array of 4 parallelogram filters.
    */
  def apply(f1: Matrix): Array[Matrix] = {
    val y1 = new Array[Matrix](4)

    // Obtain fan filters from diamond filters.

    // Modulation operation:
    y1(0) = Modulate.rows(f1)
    y1(1) = Modulate.columns(f1)

    // Transpose operation:
    y1(2) = y1(0).transpose
    y1(3) = y1(1).transpose

    // Obtain parallelogram filters from the fan filters by shearing.
    for (i <- 0 until 4) {
      val shearMatrix = i match {
        case 0 => R1
        case 1 => R2
        case 2 => R3
        case 3 => R4
        case x => throw new Exception("bad code")
      }
      y1(i) = Shear(y1(i), shearMatrix)
    }
    y1
  }
}
