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

/** An on-center, off-surround filter, implemented as a narrow center Gaussian
  * minus a broad surround Gaussian. Each Gaussian is normalized to sum to 1.
  *
  * @author Greg Snider
  */

object CenterSurroundFilter {
  /**
   * Create an on-center, off-surround filter.
   *
   * @param centerSigma Sigma of center Gaussian.
   * @param surroundSigma Sigma of surround Gaussian.
   * @return Kernel
   */
  def apply(centerSigma: Float, surroundSigma: Float): ScalarField = {
    require(centerSigma < surroundSigma, "center must be narrower than surround")
    val surround = GaussianFilter(surroundSigma)
    val center = GaussianFilter(surround.fieldShape(0), centerSigma)
    center - surround
  }
}
