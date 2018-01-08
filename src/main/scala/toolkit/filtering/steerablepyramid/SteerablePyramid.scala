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

package toolkit.filtering.steerablepyramid

import libcog._

/**
  * Steerable pyramid, described in"The Steerable Pyramid: A Flexible Architecture
  * for	 Multi-Scale Derivative Computation", E P Simoncelli and W T Freeman,
  *	Second Int'l Conf on Image Processing, 1995.
  *
  *	Also see "The Design and Use of Steerable Filters" by Freeman and Adelson,
  *	and "Shiftable Multiscale Transforms" by Simoncelli et al.
  *
  *	@param input Input image to pyramid.
  *	@param filters Filters to be used in the pyramid.
  *	@param levels Number of levels of filter banks.
  *
  * @author Greg Snider
  */

class SteerablePyramid(input: ScalarField,
                       filters: Filters = FiltersOrder3, levels: Int = 2)
{
  val bp = BorderClamp // Border Policy
  val low0 = input.convolve(filters.lowpass0, bp)
  val high0 = input.convolve(filters.highpass0, bp)
  val restored = (filterBank(low0).convolve(filters.lowpass0, bp) +
        high0.convolve(filters.highpass0, bp)).max(0f).min(1f)

  def filterBank(in: ScalarField, level: Int = 0): ScalarField = {
    if (level >= levels)
      in
    else {
      val L = (in.convolve(filters.lowpass, bp)).downsample()
      val B = Array.tabulate[ScalarField](filters.bandpass.length) {
        i => in.crossCorrelate(filters.bandpass(i), bp)
      }
      // Reconstruction
      val rB = Array.tabulate[ScalarField](filters.bandpass.length) {
        i => B(i).convolve(filters.bandpass(i), bp)
      }
      val rL =
        filterBank(L, level + 1).upsample().convolve(filters.lowpass, bp)
      rL + rB.reduceLeft(_ + _)
    }
  }
}

