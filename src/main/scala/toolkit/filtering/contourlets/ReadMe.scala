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

/** This package implements the nonsubsampled contourlet transform.
  *
  * It is based on the Matlab nonsubsampled contourlet toolbox available here:
  *
  * http://www.mathworks.com/matlabcentral/fileexchange/10049-nonsubsampled-contourlet-toolbox
  *
  * Here's a map of Matlab files to corresponding Scala files in this
  * directory:
  * {{{
  *    atrousfilters.m   AtrousFilters
  *    dfilters.m        DirectionalFilters
  *    dmaxflat.m        DiamondMaxFlatFilters
  *    mctrans.m         McClellanTransform
  *    modulate2.m       Modulate
  *    nsctdec.m         ContourletTransform
  *    nsdfbdec.m        DirectionalFilterbank
  *    parafilters.m     ParallelogramFilters
  *    resamplz.m        Shear
  * }}}
  *
  * @author Greg Snider
  */
class ReadMe
