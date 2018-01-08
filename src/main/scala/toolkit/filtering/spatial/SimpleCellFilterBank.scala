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

package toolkit.filtering.spatial

import libcog._
import scala.math.Pi

/** A bank of simple cell filters with equal angular spread.
  *
  * @author Greg Snider
  */
object SimpleCellFilterBank {

  /** Create a bank of simple cell filters.
    *
    * @param filters Number of filters.
    * @param longSigma Width of filter in "long" direction.
    * @param shortSigma Width of filter in "short" direction.
    * @return Array of simple cell filters with equal angular spread.
    */
  def apply(filters: Int, longSigma: Float = 4f, shortSigma: Float = 1f):
    Array[ScalarField] =
  {
    Array.tabulate[ScalarField](filters) {
      (orientation) =>
        SimpleCellFilter(angle(orientation, filters), longSigma, shortSigma)
    }
  }

  private def angle(index: Int, filters: Int): Float =
    ((index * 2 * Pi) / filters).toFloat
}