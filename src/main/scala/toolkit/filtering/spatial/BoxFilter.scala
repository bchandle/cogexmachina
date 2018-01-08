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

/** Factory for creating 2D box filters.
  *
  * @author Greg Snider
  */
object BoxFilter {

  /** Create a `size` x `size` box filter. */
  def apply(size: Int): ScalarField = {
    require((size > 0) && (size % 2 == 1), "box filters must have odd size")
    val pixels = size * size
    ScalarField(size, size, (row, col) => 1f / pixels)
  }
}