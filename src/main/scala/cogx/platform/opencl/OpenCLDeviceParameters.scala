/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogx.platform.opencl

/** Parameters describing the capabilities of an OpenCL device. Useful when
  * generating code.
  *
  * @author Greg Snider
  */
private[cogx]
class OpenCLDeviceParameters(device: OpenCLDevice) {
  val globalMemCachelineSize = device.clDevice.getGlobalMemCachelineSize
  val globalMemCacheSize = device.clDevice.getGlobalMemCacheSize
  val globalMemSize = device.clDevice.getGlobalMemSize
  val localMemSize = device.clDevice.getGlobalMemSize
  val maxBufferSize = device.clDevice.getMaxMemAllocSize
}
