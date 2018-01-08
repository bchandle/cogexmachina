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
class RandomVectorSensor private (fieldShape:Shape,
                                  tensorShape:Shape,
                                  nextFcn:(Random)=>Float,
                                  seed:Option[Long]){
  private val rng = seed match {
    case None => new Random()
    case Some(s) => new Random(s)
  }

  private val nextIter:()=>Option[Iterator[Vector]] = () => Some(new Iterator[Vector]{
    private val vec = new Vector(tensorShape(0))
    private val vecData = vec.asArray
    def hasNext = true
    def next() = {
      for(i<-0 until vecData.length){vecData(i)= nextFcn(rng)}
      vec
    }
  })

  private val reset = () => rng.reset()

  val sensor = new VectorSensor(fieldShape, tensorShape, nextIter, reset)
}

object RandomVectorSensor{
  def apply(fieldShape:Shape, tensorShape:Shape,
            nextFcn:(Random)=>Float = (r)=>r.nextFloat(),
            seed:Option[Long] = None) = {
    new RandomVectorSensor(fieldShape, tensorShape, nextFcn, seed).sensor
  }
}
