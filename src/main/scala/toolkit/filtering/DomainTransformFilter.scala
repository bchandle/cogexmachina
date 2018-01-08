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

package toolkit.filtering

import libcog._

/** Implements the normalized convolution version of the "domain transform"
  * filter. This filter is "edge-aware" and able to filter without blurring
  * edges. For a complete description, see "Domain transform for edge-aware
  * image and video processing," Gastal and Oliveira, 2011.
  *
  * @author Greg Snider
  */
object DomainTransformFilter extends FloatMath {

  /** Filter a 2D tensor field with the normalized convolution version of the
    * domain transform filter. This smoothes textures while preserving edges.
    * It is similar to a bilateral filter, but much faster.
    *
    * @param input The color field to be filtered.
    * @param spatialSigma Spatial extent of nonlinear filter.
    * @param rangeSigma Range extent of nonlinear filter (same concept as range
    *        in bilateral filters).
    * @param iterations Number of iterations to run filter. Usually converges
    *        in 10 to 15 iterations, but 3 iterations is good enough for most
    *        applications.
    * @return Filtered version of input.
    */
  def apply(input: Field,
            spatialSigma: Float,
            rangeSigma: Float,
            iterations: Int = 3): Field =
  {
    input match {
      case color: ColorField =>
        filterColor(color, spatialSigma, rangeSigma, iterations)
      case scalar: ScalarField =>
        filterTensor(scalar, spatialSigma, rangeSigma, iterations)
      case vector: VectorField =>
        filterTensor(vector, spatialSigma, rangeSigma, iterations)
      case matrix: MatrixField =>
        filterTensor(matrix, spatialSigma, rangeSigma, iterations)
      case _ =>
        throw new Exception("Illegal call")
    }
  }


  /** Filter a color image with the normalized convolution version of the
    * domain transform filter. This smoothes textures while preserving edges.
    * It is similar to a bilateral filter, but much faster.
    *
    * @param input The color field to be filtered.
    * @param spatialSigma Spatial extent of nonlinear filter.
    * @param rangeSigma Range extent of nonlinear filter (same concept as range
    *        in bilateral filters).
    * @param iterations Number of iterations to run filter. Usually converges
    *        in 10 to 15 iterations, but 3 iterations is good enough for most
    *        applications.
    * @return Filtered version of input.
    */
  private def filterColor(input: ColorField,
                          spatialSigma: Float,
                          rangeSigma: Float,
                          iterations: Int = 3): ColorField =
  {
    val domainTransformRows =
      input.domainTransformRows(spatialSigma, rangeSigma)
    val domainTransformColumns =
      input.transpose.domainTransformRows(spatialSigma, rangeSigma)

    var smooth: ColorField = input
    for (i <- 0 until iterations) {
      // Sigma for this iteration, eqn (14)
      val sigma = spatialSigma * sqrt(3f) * pow(2, iterations - (i + 1)) /
              sqrt(pow(4, iterations) - 1)
      // Radius of box filter with the desired variance.
      val boxRadius = sqrt(3f) * sigma

      smooth = smooth.domainFilterRows(domainTransformRows, boxRadius)
      smooth = smooth.domainFilterColumns(domainTransformColumns, boxRadius)
    }
    smooth
  }

  /** Filter a 2D tensor field with the normalized convolution version of the
    * domain transform filter. This smoothes textures while preserving edges.
    * It is similar to a bilateral filter, but much faster.
    *
    * @param input The color field to be filtered.
    * @param spatialSigma Spatial extent of nonlinear filter.
    * @param rangeSigma Range extent of nonlinear filter (same concept as range
    *        in bilateral filters).
    * @param iterations Number of iterations to run filter. Usually converges
    *        in 10 to 15 iterations, but 3 iterations is good enough for most
    *        applications.
    * @return Filtered version of input.
    */
  private def filterTensor(input: Field,
            spatialSigma: Float,
            rangeSigma: Float,
            iterations: Int = 3): Field =
  {
    val domainTransformRows =
      input.domainTransformRows(spatialSigma, rangeSigma)
    val domainTransformColumns =
      input.transpose.domainTransformRows(spatialSigma, rangeSigma)

    require(input.tensorShape == domainTransformRows.tensorShape)

    var smooth: Field = input
    for (i <- 0 until iterations) {
      // Sigma for this iteration, eqn (14)
      val sigma = spatialSigma * sqrt(3f) * pow(2, iterations - (i + 1)) /
              sqrt(pow(4, iterations) - 1)
      // Radius of box filter with the desired variance.
      val boxRadius = sqrt(3f) * sigma

      smooth = smooth.domainFilterRows(domainTransformRows, boxRadius)
      smooth = smooth.transpose.
              domainFilterRows(domainTransformColumns, boxRadius).transpose
    }
    smooth
  }
}
