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
package toolkit.computervision.annotation

import libcog._

/**
  * A user function class for writing a circle to a `ScalarField`. The shape of
  * the output must be defined at definition time. The position and size of the
  * circle are specified by `ScalarField`s which can change over time.
  *
  * Example:
  *
  * val circleWriter = new CircleWriter(Shape(10,10))
  * val row = ScalarField(5f)
  * val column = ScalarField(5f)
  * val radius = ScalarField(5f)
  * val fieldWithCircle = cirlceWriter(row, column, radius)
   *
  * Note that `CircleWriter`s contains internal state that is mutated each time
  * the operator is called, requiring a different `CircleWriter` for each cross
  * you want to generate.
  *
  * @author Matthew Pickett
  */
class CircleWriter(fieldShape:Shape, points:Int) extends Operator{
  require(fieldShape.dimensions == 2)

  private val rows = fieldShape(0)
  private val columns = fieldShape(1)

  private var prevRadius = 2
  private var prevRowCenter = prevRadius
  private var prevColCenter = prevRadius

  private def writeCircle(value:Float, rCenter:Int, cCenter:Int, radius:Int,
                          out:ScalarFieldWriter){
    for(i <- 0 until points){
      val theta = i.toFloat/points*2*3.1415f
      val r = rCenter+(math.sin(theta)*radius).toInt
      val c = cCenter + (math.cos(theta)*radius).toInt
      if (r >=0 && r < rows && c >=0 && c < columns)
        out.write(r,c, value)
    }
  }

  def compute(row:ScalarFieldReader, column:ScalarFieldReader,
              radius:ScalarFieldReader, out:ScalarFieldWriter){
    require(row.fieldShape.dimensions == 0, "radius must be a 0D scalar field")
    require(column.fieldShape.dimensions == 0, "radius must be a 0D scalar field")
    require(radius.fieldShape.dimensions == 0, "radius must be a 0D scalar field")

    out.setShape(fieldShape)

    //erase old circle
    writeCircle(0f, prevRowCenter, prevColCenter, prevRadius, out)

    //get new properties
    val newRowCenter:Int = math.round(row.read())
    val newColCenter:Int = math.round(column.read())
    val newRadius:Int = math.round(radius.read())

    //write new circle
    writeCircle(1f, newRowCenter, newColCenter, newRadius, out)

    //udpate state
    prevRadius = newRadius
    prevRowCenter = newRowCenter
    prevColCenter = newColCenter
  }
}
