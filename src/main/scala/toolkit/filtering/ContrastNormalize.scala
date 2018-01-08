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

package toolkit.filtering

import libcog._
import toolkit.filtering.spatial.GaussianFilter

/**
  * 
  * @author Matthew Pickett
  */
object ContrastNormalize {
  def apply(x:VectorField, scale:Float):VectorField = {
    require(scale > 0.3f, s"Scale not valid: $scale")
//    require(size%2 == 1, "domain size must be odd")
//    val points:Float = size*size
    //val meanFilter = ScalarField(size, size, (i,j)=>1f/points)
    val meanFilter = GaussianFilter(scale)
    val localMean = x.convolve(meanFilter, BorderClamp)
    val localStdev = (x-localMean).sq.convolve(meanFilter, BorderClamp).max(1e-3f).sqrt
    (x-localMean)/localStdev
    //(x-localMean)/localStdev


    //val mag = (x-localMean).sq
    //val localMag = mag.convolve(summingFilter, BorderZero)
    //val denom = (1 + scale*localMag).pow(power)
    //(x-localMean)/denom
  }
}