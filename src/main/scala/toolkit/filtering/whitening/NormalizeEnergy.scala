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
import toolkit.filtering.spatial.GaussianFilter

/** Normalize the "energy" in a scalar field (which is assumed to be an image).
  *
  * @author Greg Snider
  */

object NormalizeEnergy {
  /** Normalize the energy in an image.
    *
    * @param image The image to be normalized. It is assumed that the pixelsin
    *        the image vary from 0.0 to 1.0.
    * @param patchSize Size of convolutional domain over which to normalize.
    */

  def apply(image: Field, patchSize: Int): Field = {
    val Tiny = 0.1f
    val sigma = patchSize / 6f
    val localMean = image.convolve(GaussianFilter(sigma), BorderClamp)
    val highPass = image - localMean
    val localVariance =
      (highPass * highPass).convolve(GaussianFilter(sigma), BorderClamp)
    val normalized = highPass / (localVariance + Tiny)
    normalized
  }
}
