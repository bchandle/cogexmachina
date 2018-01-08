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

/** Creates a mask for an image with 1's in the center of the "valid" region
  * (for convolution or cross correlation) and 0's around the border.
  *
  * @author Greg Snider
  */

object BorderMask {

  /** Create a mask for blocking out border pixels where convolution with
    * a filter is not valid (since the convolution extends beyond the border
    * of the image).
    *
    * Note that the size of the border equals half the filter size rounded
    * down.
    *
    * @param fieldShape Shape of the image to be masked.
    * @param filterSize Size of filter that implies the valid region of a
    *        convolution.
    * @return Field with 1's in the center, 0's around border.
    */
  def apply(fieldShape: Shape, filterSize: Int): ScalarField = {
    require(fieldShape.dimensions == 2, "only 2D supported for now")
    require(filterSize % 2 == 1, "filter size must be odd")
    val borderSize = filterSize / 2
    val rows = fieldShape(0)
    val columns = fieldShape(1)
    val borderMask = ScalarField(rows, columns, (row, col) =>
      if (row < borderSize) 0f
      else if (row >= (rows - borderSize)) 0f
      else if (col < borderSize) 0f
      else if (col >= (columns - borderSize)) 0f
      else 1f
    )
    borderMask
  }

  /** Compute the number of valid pixels for an input image if convolved with
    * a filter.
    *
    * @param fieldShape Shape of input image.
    * @param filterSize Size of filter to be convolved with input image.
    * @return Number of valid points in the output of the convolution.
    */
  def validPixels(fieldShape: Shape, filterSize: Int): Int = {
    require(fieldShape.dimensions == 2, "only 2D supported for now")
    require(filterSize % 2 == 1, "filter size must be odd")
    val totalBorder = filterSize - 1
    Shape(fieldShape(0) - totalBorder, fieldShape(1) - totalBorder).points
  }

  /** Compute the number of invalid border pixels for an input image if
    * convolved with a filter.
    *
    * @param fieldShape Shape of input image.
    * @param filterSize Size of filter to be convolved with input image.
    * @return Number of border points in the output of the convolution.
    */
  def borderPixels(fieldShape: Shape, filterSize: Int): Int = {
    require(fieldShape.dimensions == 2, "only 2D supported for now")
    require(filterSize % 2 == 1, "filter size must be odd")
    fieldShape.points - validPixels(fieldShape, filterSize)
  }
}
