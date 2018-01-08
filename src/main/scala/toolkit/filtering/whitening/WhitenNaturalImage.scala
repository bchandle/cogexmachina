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

/** Whiten a natural image.
  *
  * Natural images have a power spectrum well approximated by 1 / f**2. This
  * function flattens that spectrum to facilitate further processing. The
  * algorithm is from Bruno Olshausen:

  * http://redwood.berkeley.edu/bruno/npb261b/lab2/lab2.html
  *
  * @author Greg Snider
  */
object WhitenNaturalImage  {

  /** Whiten a natural image.
    *
    * XXX A bug in normalized convolution must be fixed for this to work
    * correctly.
    *
    * @param image Image to be whitened
    * @return Whitened image.
    */
  def apply(image: ScalarField): ScalarField = {
    val filter = WhiteningFilter(image.fieldShape)
    image.convolve(filter, BorderClamp)
    // TODO
    // Bug in normalized convolution. Fix, then substitute the following XXX
    //convolveNormalized(image, filter)
  }
}