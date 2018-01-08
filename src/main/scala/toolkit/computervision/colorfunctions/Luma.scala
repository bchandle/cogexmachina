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
package toolkit.computervision.colorfunctions

import cogx._

/** Convert a vector-format color field to greyscale using NTSC luma transform.
  *
  * @author Ben Chandler
  */
object Luma {
  def apply(input: VectorField): ScalarField = {
    require(input.tensorShape == Shape(3))
    0.3f*input.vectorElement(0) + 0.59f*input.vectorElement(1) + 0.11f*input.vectorElement(2)
  }
}


