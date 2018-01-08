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
import SamplingMatrices._
import toolkit.filtering.MatlabFunctions
import toolkit.filtering.frequency.FilterFFT

/** Directional filterbank and its inverse.
  *
  * Implements the filter construction part of nsdfbdec.m and nsdfbrec.m,
  * but does not do the convolution. Although the matlab code supports an
  * arbitrary number of channels (that are a power of 2), I cut this off at
  * 8 channels. This makes the code much more readable. If you need more
  * channels, you may add the code for that.
  *
  * @author Greg Snider
  */
object DirectionalFilterbank extends Logarithm with MatlabFunctions {

  /** Diamond filters, space domain. */
  val diamond = Array[Matrix](
    DiamondFilter.lowpass / sqrt(2), DiamondFilter.highpass / sqrt(2)
  )
  val diamondInverse = Array[Matrix](
    DiamondFilter.inverseLowpass / sqrt(2), DiamondFilter.inverseHighpass / sqrt(2)
  )

  /** Fan filters, space domain. */
  val fan: Array[Matrix] = diamond.map(Modulate.columns(_))
  val fanInverse: Array[Matrix] = diamondInverse.map(Modulate.columns(_))

  /** Checkerboard filters, space domain. */
  val checkerboard: Array[Matrix] = fan.map(Resample(_, Q1))
  val checkerboardInverse: Array[Matrix] = fanInverse.map(Resample(_, Q1))

  /** Parallelogram filters, space domain. */
  val parallelogram: Array[Matrix]  = Array.concat(
    ParallelogramFilters(diamond(0)), ParallelogramFilters(diamond(1))
  )
  val parallelogramInverse: Array[Matrix]  = Array.concat(
    ParallelogramFilters(diamondInverse(0)), ParallelogramFilters(diamondInverse(1))
  )

  /** Octet filters, space domain. */
  val octet = parallelogram.map(filter => Resample(filter, I2))
  val octetInverse = parallelogramInverse.map(filter => Resample(filter, I2))

  /** Create a set of directional filters in the frequency domain.
    *
    * @param shape Shape of the images to be processed by the filterbank.
    * @param channels Number of channels (directions) supported by the
    *        filterbank. Legal values are 2, 4, 8.
    * @return An array of filters, one per channel, in the frequency domain.
    *        Each filter will have the shape `shape`. The filters are
    *        zero-phase, so they are represented with matrices rather than
    *        complex matrices.
    */
  def apply(shape: Shape, channels: Int): Array[Matrix] =
  {
    require(shape.dimensions == 2, "only 2D filters are supported")
    require(isPowerOf2(channels), "number of channels must be a power of 2")
    require(isPowerOf2(shape(0)), "image size must be a power of 2")
    require(shape(0) == shape(1), "only square images are supported now")
    require(channels >= 2, "must have at least 2 channels")

    // Level 1 filters:
    val filter1 = fan.map(filter => fft(shape, filter))

    // Level 2 filters
    lazy val filter2 = Array.tabulate(4) {
      i => filter1(i / 2) :* fft(shape, checkerboard(i % 2))
    }

    // Level 3 filters
    lazy val filter3 = Array(
      filter2(0) :* fft(shape, octet(0)),
      filter2(0) :* fft(shape, octet(4)),
      filter2(1) :* fft(shape, octet(5)),
      filter2(1) :* fft(shape, octet(1)),
      filter2(2) :* fft(shape, octet(2)),
      filter2(2) :* fft(shape, octet(6)),
      filter2(3) :* fft(shape, octet(7)),
      filter2(3) :* fft(shape, octet(3))
    )

    channels match {
      case 2 => filter1
      case 4 => filter2
      case 8 => filter3
      case x => throw new Exception("not supported")
    }
  }

  /** Create a set of inverse directional filters in the frequency domain. This
    * is the inverse of apply.
    *
    * @param shape Shape of the images to be processed by the filterbank.
    * @param channels Number of channels (directions) supported by the
    *        filterbank.  Legal values are 2, 4, 8.
    * @return An array of filters, one per channel, in the frequency domain.
    *        Each filter will have the shape `shape`. The filters are
    *        zero-phase, so they are represented with matrices rather than
    *        complex matrices.
    */
  def inverse(shape: Shape, channels: Int): Array[Matrix] = {
    // Level 1 filters
    val filter1 = fanInverse.map(filter => fft(shape, filter))

    // Level 2 filters
    lazy val filter2 = Array.tabulate(4) {
      i => filter1(i / 2) :* fft(shape, checkerboardInverse(i % 2))
    }

    // Level 3 filters
    lazy val filter3 = Array(
      filter2(0) :* fft(shape, octetInverse(0)),
      filter2(0) :* fft(shape, octetInverse(4)),
      filter2(1) :* fft(shape, octetInverse(5)),
      filter2(1) :* fft(shape, octetInverse(1)),
      filter2(2) :* fft(shape, octetInverse(2)),
      filter2(2) :* fft(shape, octetInverse(6)),
      filter2(3) :* fft(shape, octetInverse(7)),
      filter2(3) :* fft(shape, octetInverse(3))
    )

    channels match {
      case 2 => filter1
      case 4 => filter2
      case 8 => filter3
      case x => throw new Exception("not supported")
    }
  }

  /** Compute square root of a Float. */
  private def sqrt(x: Float) =
    math.sqrt(x).toFloat

  /** Compute FFT of a filter.
    *
    * @param shape Shape of the Frequency domain filter.
    * @param filter The filter to be transformed.
    * @return FFT of `filter`.
    */
  private def fft(shape: Shape, filter: Matrix): Matrix =
    FilterFFT(shape, filter).realPart

}
