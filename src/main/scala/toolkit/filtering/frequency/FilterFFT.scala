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

package toolkit.filtering.frequency

import libcog._

/** Takes the FFT of a convolution filter. This does not take into account
  * boundary effects--that must be done before calling by using, for example,
  * PadFFT.
  *
  * @author Greg Snider
  */
object FilterFFT extends Logarithm {
  /** The minimum FFT size is a function of the GPU memory block size (ughh).
    * Need a better way to capture that. XXX
    */
  val MinFFTSize = 32

  /** Take FFT of a real, centered `filter` producing a Fourier transform
    *  of shape `shape`.
    *
    * @param shape The shape of the output Fourier transform. Each dimension
    *        must be a power of 2.
    * @param filter The filter to be transformed. This must be of odd size
    *        and centered.
    * @return The Fourier transform of the filter.
    */
  def apply(shape: Shape, filter: Matrix): ComplexMatrix =
    apply(shape, new ComplexMatrix(filter))

  /** Take FFT of a complex, centered `filter` producing a Fourier transform
    *  of shape `shape`.
    *
    * @param shape The shape of the output Fourier transform. Each dimension
    *        must be a power of 2.
    * @param filter The filter to be transformed. This must be of odd size
    *        and centered.
    * @return The Fourier transform of the filter.
    */
  def apply(shape: Shape, filter: ComplexMatrix): ComplexMatrix = {
    require(shape.dimensions == 2,
      "StaticConvolution currently only supported for 2D.")
    require(isPowerOf2(shape(0)), "FFT requires powers of 2 for sizes")
    require(isPowerOf2(shape(1)), "FFT requires powers of 2 for sizes")
    require(filter.rows % 2 == 1, "StaticConvolution kernel size must be odd.")
    require(filter.columns % 2 == 1, "StaticConvolution kernel size must be odd.")

    val rows = shape(0)
    val columns = shape(1)
    val halfRows = filter.rows / 2
    val halfColumns = filter.columns / 2

    // Compute size of FFT required meeting algorithm requirement and GPU
    // memory coalescing requirement.
    val fftRows = MinFFTSize max Logarithm.roundUpPowerOf2(rows)
    val fftColumns = MinFFTSize max Logarithm.roundUpPowerOf2(columns)

    // Expand and shift the kernel.
    val bigKernel =
      filter.expand(fftRows, fftColumns, false).shiftCyclic(-halfRows, -halfColumns)
    val frequencyDomainKernel = FFT2D.transform(bigKernel)
    frequencyDomainKernel
  }

  /** Take inverse FFT of a complex frequency domain `filter` producing the
    * spatial version of the filter. This undoes shifting and expansion that
    * was done in the apply methods.
    *
    * @param shape The shape of the output spatial filter. Each dimension
    *        must be less than or equal to the corresponding dimension in
    *        filter
    * @param frequencyFilter The frequency filter to be inverse transformed.
    * @return The spatial domain version of the frequency filter.
    */
  def inverse(shape: Shape, frequencyKernel: ComplexMatrix): ComplexMatrix = {
    require(shape.dimensions == 2,
      "StaticConvolution currently only supported for 2D.")
    val spectralShape = frequencyKernel.shape
    require(isPowerOf2(spectralShape(0)), "FFT requires powers of 2 for sizes")
    require(isPowerOf2(spectralShape(1)), "FFT requires powers of 2 for sizes")
    require(shape(0) == shape(1), "StaticConvolution kernel must be square.")
    require(shape(0) % 2 == 1, "StaticConvolution kernel size must be odd.")
    val halfSize = shape(0) / 2

    val scale: Float = 1.0f / (frequencyKernel.shape.points)

    val spaceKernel = FFT2D.inverseTransform(frequencyKernel).
            shiftCyclic(halfSize, halfSize).trim(shape(0), shape(1))
    spaceKernel * scale
  }
}

