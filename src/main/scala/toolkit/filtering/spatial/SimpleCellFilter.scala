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

/** A simple cell filter, using a simple cell model frequently found in the
 * works of Steve Grossberg.
 *
 * @author Greg Snider
 */

object SimpleCellFilter {
  /** Create a horizontal simple cell rotated to "angle" with sigma in the
    * horizontal direction of "horizSigma", sigma in the vertical direction
    * of "vertSigma" (which will normally be smaller than "horizSigma") and
    * offset from the horizontal axis, "shift".
    */
  def apply(angle: Float, horizSigma: Float = 2f, vertSigma: Float = 1f): ScalarField =
  {
    toolkit.filtering.spatial.StickFilter(angle, horizSigma, vertSigma, vertSigma / 2) -
            toolkit.filtering.spatial.StickFilter(angle, horizSigma, vertSigma, -vertSigma / 2)
  }
}