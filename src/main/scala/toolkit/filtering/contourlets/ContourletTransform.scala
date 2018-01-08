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

package toolkit.filtering.contourlets

import libcog._
import toolkit.filtering.frequency.FilterFFT

/** Implements a simple Contourlet transform and its inverse.
  *
  * This supports only a single level of decomposition, since that's all that's
  * needed for my application. You may extend this if needed, but it will be
  * tricky since it involves an "a trous" convolution (if you have to ask...).
  *
  * Implements nsctdec.m and nsctrec.m
  *
  * @author Greg Snider
  */
object ContourletTransform extends Logarithm {

  /** Apply a single-level Contourlet transform to an image.
    *
    * @param image The image to be transformed.
    * @param channels Number of directional filters (contourlet filters) to use
    *        in the transform.
    * @return A (scalar field, complex vector field) tuple. The scalar field
    *        is a lowpassed version of the input and carries the information
    *        not analyzed by the contourlet filters. The complex vector field
    *        carries the compacted contourlet coefficients. In the
    *        complex vector field, the number of vector elements is
    *        equal to half the number of channels. The real and imaginary parts
    *        of the components each contain the coefficients for one of
    *        the contourlet channels. This packing of coefficients into a single
    *        complex vector field is done for efficiency.
    */
  def apply(image: ScalarField, channels: Int): (ScalarField, ComplexVectorField) = {
    val shape = image.fieldShape
    require(shape.dimensions == 2, "contourlet transform is 2D")
    require(shape(0) == shape(1), "only square images are supported")
    require(isPowerOf2(shape(0)), "image size must be power of 2")
    require(Array(2, 4, 8) contains channels, "illegal number of channels")
    val rows = shape(0)
    val columns = shape(1)
    val imageFFT = image.fft

    // Construct filters. Note that all filters are zero phase, and that the
    // directional filters are convolved with a highpass filter to extract
    // the contourlets
    val highpass: Matrix = FilterFFT(shape, AtrousFilter.highpass).realPart
    val bandpass: Array[Matrix] =
      DirectionalFilterbank(shape, channels).map(_ :* highpass)

    // Extract the lowpass part of the image
    val lowpass: Matrix = FilterFFT(shape, AtrousFilter.lowpass).realPart
    val lowpassOut = (imageFFT * ComplexField(lowpass)).fftInverse.realPart

    // We pack up the frequency domain filters into a single complex vector
    // field. The order is (bandpass0, bandpass1, ...)
    // Note that the bandpass filters have already been multiplied by the
    // highpass filter, so the highpass is not used by the transform.
    val complexFilter = ComplexVectorField(rows, columns,
      (row, col) =>
        ComplexVector(channels / 2,
          i => Complex(bandpass(2*i + 1)(row, col), bandpass(2*i)(row, col))
        )
    )

    // Do the convolutions in the frequency domain
    val bandpassOut: ComplexVectorField = (complexFilter * imageFFT).fftInverse
    (lowpassOut, bandpassOut)
  }

  /** Apply the inverse Contourlet transform. This inverts apply.
    *
    * @param lowpass Lowpass output of apply
    * @param coefficients A complex vector field
    *        carries the compacted contourlet coefficients. In the
    *        complex vector field, the number of vector elements is
    *        equal to half the number of channels. The real and imaginary parts
    *        of the components each contain the coefficients for one of
    *        the contourlet channels. This packing of coefficients into a single
    *        complex vector field is done for efficiency.
    * @return Reconstructed image
    */
  def inverse(lowpass: ScalarField, coefficients: ComplexVectorField): ScalarField = {
    val shape = lowpass.fieldShape
    val channels = coefficients.tensorShape(0) * 2
    require(shape.dimensions == 2, "contourlet transform is 2D")
    require(shape(0) == shape(1), "only square images are supported")
    require(isPowerOf2(shape(0)), "image size must be power of 2")
    require(Array(2, 4, 8) contains channels, "illegal number of channels")
    val rows = shape(0)
    val columns = shape(1)

    // Construct inverse filters. Note that all filters are zero phase.
    val highpass: Matrix = FilterFFT(shape, AtrousFilter.inverseHighpass).realPart
    val bandpass: Array[Matrix] =
      DirectionalFilterbank.inverse(shape, channels).map(_ :* highpass)

    // We pair up the direction filters and stuff them into complex fields.
    // This cuts the number of convolutions necessary in half.
    val complexFilters = ComplexVectorField(rows, columns,
      (row, col) =>
        ComplexVector(channels / 2,
          i => Complex(bandpass(2*i + 1)(row, col), bandpass(2*i)(row, col))
        )
    )

    // Do convolutions
    val conv = (complexFilters * coefficients).fftInverse

    // Sum them up.
    val sum = conv.reduceSum

    // Add real and imaginary parts of sum to get result.
    val reconstruction = sum.realPart + sum.imaginaryPart + lowpass
    reconstruction
  }
}
