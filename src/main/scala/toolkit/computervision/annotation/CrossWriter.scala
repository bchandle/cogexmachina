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
  * A user function class for writing a cross to a `ScalarField`. The shape of
  * the output must be defined at definition time. The position and size of the
  * cross are specified by `ScalarField`s which can change over time.
  *
  * Example:
  *
  * val crossWriter = new CrossWriter(Shape(10,10))
  * val row = ScalarField(5f)
  * val column = ScalarField(5f)
  * val radius = ScalarField(5f)
  * val fieldWithCross = crossWriter(row, column, radius)
   *
  * Note that `CrossWriter`s contains internal state that is mutated each time
  * the operator is called, requiring a different `CrossWriter` for each cross
  * you want to generate.
  *
  * @author Matthew Pickett
  */
class CrossWriter(fieldShape:Shape) extends Operator{
  require(fieldShape.dimensions == 2)

  private val rows = fieldShape(0)
  private val columns = fieldShape(1)

  private var prevRadius = 1
  private var prevRowCenter = 0
  private var prevColCenter = 0

  private def writeCross(value:Float, rCenter:Int, cCenter:Int, radius:Int,
                          out:ScalarFieldWriter){
    for(i <- -radius to radius){
      val r = rCenter+i
      val c = cCenter+i
      //draw vertical line
      if (r >=0 && r < rows && cCenter >=0 && cCenter < columns)
        out.write(r,cCenter, value)
      //draw horizontal line
      if (c >=0 && c < columns && rCenter >=0 && rCenter < rows)
        out.write(rCenter, c, value)
    }
  }

  def compute(row:ScalarFieldReader, column:ScalarFieldReader,
              radius:ScalarFieldReader, out:ScalarFieldWriter){
    require(row.fieldShape.dimensions == 0, "radius must be a 0D scalar field")
    require(column.fieldShape.dimensions == 0, "radius must be a 0D scalar field")
    require(radius.fieldShape.dimensions == 0, "radius must be a 0D scalar field")

    out.setShape(fieldShape)

    //erase old cross
    writeCross(0f, prevRowCenter, prevColCenter, prevRadius, out)

    val newRadius = math.round(radius.read())
    val newRowCenter = math.round(row.read())
    val newColCenter = math.round(column.read())

    //write new cross
    writeCross(1f, newRowCenter, newColCenter, newRadius, out)

    //update state
    prevRadius = newRadius
    prevRowCenter = newRowCenter
    prevColCenter = newColCenter
  }
}
