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
package toolkit.dynamicalsystems

import cogx._

/**
  * User function which calculates the Moore-Penrose pseudo inverse of a
  * 0D `MatrixField` on the CPU
  * @author Greg Snider and Matthew Pickett
  */
object PseudoInvert extends Operator {
  // Machine epsilon from:
  // http://en.wikipedia.org/wiki/Machine_epsilon#Approximation_using_Java
  val epsilon: Float = {
    var machEps = 1.0f
    do
      machEps /= 2.0f
    while ((1.0 + (machEps / 2.0)).toFloat != 1.0)
    machEps
  }

  /** Implements the Moore-Penrose pseudo-inverse of a 0D `MatrixField`
    * @param theMat the 0D `MatrixField` to invert
    * @param out output parameter used for writing to the output target
    */
  def compute(theMat:MatrixFieldReader, out:MatrixFieldWriter){
    require(theMat.fieldShape.dimensions == 0, "matrix must be zero D")

    val rows = theMat.tensorShape(0)
    val columns = theMat.tensorShape(1)

    out.setShape(Shape(), Shape(rows,columns))

    val theMatLocal = new Matrix(rows, columns)
    theMat.read(theMatLocal)
    val (u, s, v) = theMatLocal.svd

    // Compute the tolerance
    val tolerance = epsilon * (rows max columns) * s.reduce(_ max _)

    // Take the pseudo-inverse of s.
    val inverseS = s.transpose
    val diagonalSize = inverseS.rows min inverseS.columns
    for (d <- 0 until diagonalSize)
      if (inverseS(d, d) > tolerance)
        inverseS(d, d) = 1f / inverseS(d, d)

    // Compute the pseudo-inverse
    out.write(v*inverseS*u.transpose)

  }
}
