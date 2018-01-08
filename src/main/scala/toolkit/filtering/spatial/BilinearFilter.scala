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

/** Bilinear filter.
  *
  * @author Ben Chandler
  */
object BilinearFilter {
  def apply(): ScalarField = {
    val k = Array(
      Array(0.25f, 0.50f, 0.25f),
      Array(0.50f, 1.00f, 0.50f),
      Array(0.25f, 0.50f, 0.25f)
    )

    ScalarField(3, 3, (r, c) => k(r)(c))
  }
}
