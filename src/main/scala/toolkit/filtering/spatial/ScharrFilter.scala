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

/** Factory for creating Scharr filters, useful for creating tiny
  * gradient operators.
  *
  * @author Greg Snider
  */
object ScharrFilter {

  // Differentiator in y direction (across rows)
  private val dY = Matrix(
    Array(-3f, -10f, -3f),
    Array( 0f,   0f,  0f),
    Array( 3f,  10f,  3f)
  ) / 32f

  // Differentiator in x direction (across columns)
  private val dX = Matrix(
    Array( -3f, 0f,  3f),
    Array(-10f, 0f, 10f),
    Array( -3f, 0f,  3f)
  ) / 32f

  /** Create a Scharr kernel that differentiates in y direction (across rows).
    * To use this, crossCorrelate an image with this filter.
    */
  def dy = ScalarField(3, 3, (row, col) => dY(row, col))

  /** Create a Scharr kernel that differentiates in x direction (across columns).
    * To use this, crossCorrelate an image with this filter.
    */
  def dx = ScalarField(3, 3, (row, col) => dX(row, col))

  /** Create a Scharr kernel that differentiates in x direction (across columns).
    * To use this, crossCorrelate an image with this filter.
    */
  def dColumns = dx

  /** Create a Scharr kernel that differentiates in y direction (across rows).
    * To use this, crossCorrelate an image with this filter.
    */
  def dRows = dy
}