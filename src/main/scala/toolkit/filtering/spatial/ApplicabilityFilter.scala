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

/**
  * Implements the "applicability" filter as described in "Normalized and
  * differential convolution: methods for interpolation and filtering of
  * incomplete and uncertain data," Knutsson and Westin.
  *
  * @author Greg Snider
  */

object ApplicabilityFilter {
  /** Create an applicability filter.
    *
    * For filling in missing values, the authors recommend width = 15,
    * alpha = 3, beta = 0.
    *
    * @param width The size of the kernel; this is equal to (2 * rmax - 1), where
    *        rmax is as defined in the paper.
    * @param alpha Sharpness of the applicability function; larger is sharper
    * @param beta Exponent which defines the effective width of the function;
    *        larger implies a narrower applicability function.
    */
  def apply(width: Int, alpha: Float, beta: Float): ScalarField = {
    val filter = asMatrix(width, alpha, beta)
    ScalarField(width, width, (row, col) => filter(row, col))
  }

  /** Create an applicability filter as a Matrix.
    *
    * For filling in missing values, the authors recommend width = 15,
    * alpha = 3, beta = 0.
    *
    * @param width The size of the kernel; this is equal to (2 * rmax - 1), where
    *        rmax is as defined in the paper.
    * @param alpha Sharpness of the applicability function; larger is sharper
    * @param beta Exponent which defines the effective width of the function;
    *        larger implies a narrower applicability function.
    */
  def asMatrix(width: Int, alpha: Float, beta: Float): Matrix = {
    require(alpha >= 0 && beta >= 0)
    val radius = width / 2   // same a rmax in the paper

    val fcn: (Int,Int) => Float = (r,c) => {
      val distance = math.sqrt((r-radius)*(r-radius) + (c-radius)*(c-radius)).toFloat
      val factor1 =
        if (alpha == 0 || distance == 0)
          1f
        else
          math.pow(distance, -alpha).toFloat
      val factor2 =
        if (beta == 0)
          1f
        else
          math.pow(math.cos((math.Pi * distance) / (2 * radius)), beta).toFloat
      if (distance == 0) {
        printf("factor1 = %f, factor2 = %f\n", factor1, factor2)
      }
      factor1 * factor2
    }
    Matrix(width, width, fcn)
  }
}
