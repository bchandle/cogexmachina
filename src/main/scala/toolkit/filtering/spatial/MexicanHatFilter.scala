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

/**
  * Laplacian of a Gaussian. See "Vision" by David Marr, section 2.2.
  * Note that this kernel autosizes based on the value of sigma that
  * is supplied (crudely done, but good enough).
  *
  * @author Greg Snider
  */

object MexicanHatFilter {
  def apply(sigma:Float):ScalarField = {
    val width = ((6 * sigma).toInt & 0xfffffffe) + 1
    val center = width / 2
    val sigmaSq = sigma * sigma

    val fcn:(Int, Int)=>Float = (r, c)=>{
      val distSq = (center-r)*(center-r)+(center-c)*(center-c)
      val ratio = distSq / (2 * sigmaSq)
      val result = (1 - ratio) * math.exp(-ratio)
      result.toFloat
    }
    ScalarField(width, width, fcn).normalizeL2
  }
}