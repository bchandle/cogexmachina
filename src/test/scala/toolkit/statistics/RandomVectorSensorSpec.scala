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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import cogio.fieldstate.FieldState

/**
  * @author Matthew Pickett
  */
@RunWith(classOf[JUnitRunner])
class RandomVectorSensorSpec extends FunSuite {
  def genShape() = {
    val rng = new java.util.Random
    val dims = rng.nextInt(4)
    val sizes = Array.tabulate(dims){i=>rng.nextInt(10)+1}
    val fieldShape = Shape(sizes)
    val tensorShape = Shape(rng.nextInt(10)+1)
    (fieldShape, tensorShape)
  }

  test("Sequence Correct") {
    val seedGen = new java.util.Random
    val seed = seedGen.nextLong()
    val numPoints = 10000

    val rng = new java.util.Random(seed)
    val randoms = IndexedSeq.tabulate(numPoints){i=>rng.nextFloat()}

    val (fieldShape, tensorShape) = genShape()
    val points = fieldShape.points*tensorShape.points
    val steps = numPoints/points + 1
    require(steps*points > numPoints)

    val cg = new ComputeGraph{
      val x = RandomVectorSensor(fieldShape, tensorShape, (r)=>r.nextFloat(), Some(seed))
      x.probe()
    }

    println(s"FieldShape: $fieldShape")
    println(s"TensorShape: $tensorShape")
    println(s"Seed: $seed")

    val result = try{
      cg.reset
      for(i<-0 until steps) yield {
        val res = FieldState.read(cg.read(cg.x)).data
        cg.step
        res
      }
    }
    finally{cg.release}
    val flat = result.flatten.take(numPoints)
    val eq = flat.zip(randoms).map(x=> x._1 == x._2).reduce(_&&_)

    require(eq)
  }
}
