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
package toolkit.filtering.morphology

import libcog._

/** Binary morphological closing/opening with a box probe.
  *
  * ClosingOpening applies a morphological closing followed by an
  * opening of the same scale. This sequence of operations enforces smooth
  * transitions between the two classes in the input binary image. Small
  * regions of either black or white pixels are removed, with preference given
  * to white pixels.
  *
  * @author Ben Chandler
  */
object ClosingOpening {
  def apply(input: ScalarField, scale: Int = 1): ScalarField = {
    assert(scale >= 0, "scale must be >= 0")

    Dilate(Erode(Dilate(input, scale), 2*scale), scale)
  }
}


