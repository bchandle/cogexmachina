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

package toolkit.filtering.whitening

import libcog._
import toolkit.filtering.MatlabFunctions

/** A whitening filter for natural images that flattens the spectrum (on
  * average).
  *
  * This uses the fact that natural images have an amplitude spectrum of 1/f,
  * so this is essentially a filter with the inverse spectrum. The algorithm
  * is from Bruno Olshausen:
  *
  * http://redwood.berkeley.edu/bruno/npb261b/lab2/lab2.html
  *
  * @author Greg Snider
  */
object WhiteningFilter extends MatlabFunctions with Logarithm {
  val KernelSize = 13
  val MinImageSize = 16
  require(MinImageSize > KernelSize)

  /** Create a whitening filter for a natural image.
    *
    * Note that we clip this filter to 13 x 13 because normalized convolution
    * (which we would like for whitening) isn't yet implemented for larger
    * filters.
    *
    * TODO: Stop clipping whitening filter when FFT with zero expansion is
    * supported XXX
    *
    * @param imageShape Shape of the image.
    * @param frequencyDomain True if the returned filter should be in the
    *        frequency domain, false if in the space domain (default).
    * @return A natural whitening filter (spatial), that's smaller than the
    *        smallest dimension of the image.
    */
  def apply(imageShape: Shape, frequencyDomain: Boolean = false): ScalarField = {
    require(imageShape.dimensions == 2)
    // Get the smallest dimension of image shape and round down to the nearest
    // power of 2.
    val rawSize = (imageShape(0) min imageShape(1)) max MinImageSize
    val N = if (isPowerOf2(rawSize))
      rawSize
    else
      roundUpPowerOf2(rawSize / 2)

    // Create a 1/f function in the frequency domain. Frequencies run
    // from -N/2 to N/2 - 1.
    val f = indexRange(-N / 2, N / 2 - 1)
    val (fx, fy) = meshgrid(f, f)
    val (theta, rho) = cart2pol(fx, fy)

    // Window the 1/f function with a circular Gaussian to (1) clip the corners
    // of the frequency domain; and (2) low-pass filter the highest
    // frequencies to minimize the effects of noise.
    val gauss = new Matrix(N, N)
    def sq(x: Float) = x * x
    for (row <- 0 until N; col <- 0 until N) {
      gauss(row, col) = math.exp(-0.5f * sq(rho(row, col) / (0.7f * N / 2))).toFloat
    }
    val windowedFilter = gauss :* rho
    if (frequencyDomain) {
      // Return frequency domain filter.
      ScalarField(N, N, (row, col) => windowedFilter(row, col))
    } else {
      // Now convert frequency domain filter to space domain
      val complexFilter = new ComplexMatrix(ifftshift(windowedFilter))
      val spaceFilter = fftshift(FFT2D.inverseTransform(complexFilter).realPart)

      // The filter is of even size, but we need it to be of odd size, so we
      // clip the top row and left column to create a filter that's perfectly
      // centered.
      val shift = (N - KernelSize + 1) / 2
      val filter = ScalarField(KernelSize, KernelSize,
        (row, col) => spaceFilter(row + shift, col + shift))

      // Return space domain filter.
      filter.normalizeL2
    }
  }
}
