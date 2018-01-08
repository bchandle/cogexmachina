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
  * Utilities for computing 1-D Gaussians.
  */
object GaussianDerivativeFilter {
  /**One-dimensional gaussian at distance "x" from origin, std dev "sigma". */
  private def gaussian(x: Int, sigma: Float): Float = {
    val denominator = sigma * math.sqrt(2 * math.Pi)
    val numerator = math.exp(-x * x / (2 * sigma * sigma))
    (numerator / denominator).toFloat
  }

  /**One-dimensional derivative of a gaussian at distance "x", width "sigma".*/
  private def gaussianDeriv(x: Int, sigma: Float): Float = {
    -(x / (sigma * sigma)) * gaussian(x, sigma)
  }

  /**
    * Derivative of Gaussian kernel along the y axis. Note that since we use
    * (row, column) notation for kernels, the derivative is along the first
    * index (row). Also note that we need to flip the sign of the row since it
    * descends in the downward direction in matrix notation, but ascends in
    * (x, y) notation.
    *
    * Author: Greg Snider
    */
  def dY(width: Int, sigma: Float):ScalarField = {
    val radius = width / 2
    val fcn: (Int,Int)=>Float =
      (r, c) => gaussianDeriv(radius - r, sigma) * gaussian(radius - c, sigma)
    ScalarField(width,width,fcn)
  }
  /** Auto-sized version of dY*/
  def dY(sigma:Float):ScalarField = dY((5 * sigma).toInt | 1, sigma)


  /**
    * Derivative of Gaussian kernel along the x axis. Note that since we use
    * (row, column) notation for kernels, the derivative is along the second
    * index (column). Also note that we need to flip the sign of the row since it
    * descends in the downward direction in matrix notation, but ascends in
    * (x, y) notation.
    *
    * Author: Greg Snider
    */
  def dX(width: Int, sigma: Float):ScalarField = {
    val radius = width / 2
    val fcn: (Int,Int)=>Float =
      (r, c) => gaussianDeriv(radius - c, sigma) * gaussian(radius - r, sigma)
    ScalarField(width,width,fcn)
  }
  /** Auto-sized version of dX*/
  def dX(sigma:Float):ScalarField = dX((5 * sigma).toInt | 1, sigma)

}