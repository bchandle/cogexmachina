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

/** Function which performs normalized averaging (often referred to as
  * normalized convolution) of a field.
  *
  * The result is similar to linear diffusion with Dirichlet boundary
  * constraints (where the boundary is formed by the "certain" pixels in
  * the field) but is much faster. This may not be what you want since it
  * produces very fuzzy filled-in regions for large blocks of uncertain values.
  *
  * Typical usage:
  * {{{
  *   val image: Field  // image with missing pixels
  *   val applicability = ApplicabilityFilter(width = 33, alpha = 3, beta = 0)
  *   val certainty = ScalarField(...) // 1 => pixel certain, 0 => pixel unknown
  *   val filteredImage = NormalizedConvolution(image, applicability, certainty)
  * }}}
  *
  * @author Greg Snider
  */
object NormalizedConvolution {

  /** Fill in uncertain pixels.
   *
    * @param input Input field with missing or uncertain pixels.
    * @param applicability Applicability filter to use for interpolation. The
    *        object ApplicabilityFilter provides a simple way of generating
    *        some standard filters, though you may use others.
    * @param certainty Scalar field of the same shape as `input` where each
    *        pixel specifies the certainty of the corresponding pixel in
    *        `input`. A value of 1.0 means "certain," a value of 0.0 means
    *        "unknown," with intermediate values allowable.
    * @return The input field with uncertain pixels filled in.
    */
  def apply(input: Field, applicability: Field, certainty: Field): Field = {
    require(certainty.fieldType.tensorShape.dimensions == 0,
      "certainty field must be a scalar field")
    val numerator = input.convolve(applicability, BorderZero)
    val denominator = certainty.convolve(applicability, BorderZero)
    numerator / denominator
  }
}