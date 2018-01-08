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

/** Constructor for a simple symmetric Gaussian filter.
  *
  * @author Greg Snider
  */
object GaussianFilter {
  /**Estimate a "reasonable" odd size for kernel with "sigma". */
  def size(sigma: Float): Int = {
    val rough = (6 * sigma).ceil.toInt
    // Force size to be odd
    (rough / 2) * 2 + 1
  }

  /** Create a normalized, automatically-sized Gaussian kernel. */
  def apply(sigma: Float):ScalarField = apply(size(sigma), sigma)

  /** Create a "width" x "width" Gaussian kernel with stdev "sigma". */
  def apply(width: Int, sigma: Float):ScalarField = {
    ScalarField(matrix(width, sigma))
  }

  /** Create a normalized, automatically-sized Gaussian kernel. */
  def matrix(sigma: Float): Matrix = {
    matrix(bestSize(sigma), sigma)
  }

  /** Create a "width" x "width" Gaussian kernel with stdev "sigma". */
  def matrix(width: Int, sigma: Float): Matrix = {
    require(width % 2 ==1, "kernel width must be odd")
    val center = width/2
    val sigmaSq = sigma*sigma
    val amplitude = 1.0f/(2*math.Pi*sigmaSq)
    val fcn:(Int, Int)=>Float = (r, c)=>{
      val distSq = (center-r)*(center-r)+(center-c)*(center-c)
      (amplitude*math.exp((-1.0f / (2 * sigmaSq)) * distSq)).toFloat
    }
    val matrix = Matrix(width, width, fcn)
    // Normalize the matrix so that the sum of all pixels is 1.
    var sum = 0f
    for (row <- 0 until width; col <- 0 until width) {
      sum += matrix(row, col)
    }
    matrix / sum
  }

  /** Compute the smallest odd size for a Gaussian kernel with width "sigma".
    * This uses the rule of thumb that a Gaussian kernel need only extend out
    * 3 sigma from the center in all directions to be a good approximation.
    */
  def bestSize(sigma: Float): Int = {
    val size = (6 * sigma).ceil.toInt
    if (size % 2 == 1)
      size
    else
      size + 1
  }
}
