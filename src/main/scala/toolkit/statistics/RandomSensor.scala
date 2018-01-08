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

package toolkit.statistics

import libcog._

/**
  * @author Matthew Pickett
  */
class RandomSensor private (fieldShape: Shape,
                            nextFcn:(Random)=>Float,
                            seed:Option[Long]) {
  private val rng = seed match {
    case None => new Random()
    case Some(s) => new Random(s)
  }

  private val nextIter:()=>Option[Iterator[Float]] = () => Some(new Iterator[Float]{
    def hasNext = true
    def next() = nextFcn(rng)
  })

  private val reset = () => rng.reset()

  val sensor = new Sensor(fieldShape, nextIter, reset)
}

object RandomSensor{
  def apply(fieldShape:Shape,
            nextFcn:(Random)=>Float = (r)=>r.nextFloat(),
            seed:Option[Long] = None) = {
    new RandomSensor(fieldShape, nextFcn, seed).sensor
  }
}
