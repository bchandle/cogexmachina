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

import libcog._
import toolkit.filtering.MatlabFunctions

/** Diamond max flat filters of various orders.
  *
  * @author Greg Snider
  */
object DiamondMaxFlatFilter extends MatlabFunctions {

  /** Create a diamond max flat filter.
    *
    * @param order Order of the filter, must be in {1, 2, 3, 4, 5, 6, 7}.
    * @param center Center value of the filter, must be either 0 or 1
    */
  def apply(order: Int, center: Int): Matrix = {
    require(order >= 1 && order <= 7, "illegal filter order")
    require(center == 0 || center == 1, "illegal center value")

    order match {
      case 1 =>
        val h = Matrix(
          Array(0f, 1f, 0f),
          Array(1f, 0f, 1f),
          Array(0f, 1f, 0f)
        ) / 4
        h(1, 1) = center
        h

      case 2 =>
        var h = Matrix(
          Array(0f, -1f, 0f),
          Array(-1f, 0f, 10f),
          Array( 0f, 10f, 0f)
        )
        h = concat(h, fliplr(clipLastColumn(h)))
        h = stack(h, flipud(clipLastRow(h))) / 32
        h(2, 2) = center
        h

    case 3 =>
      var h = Matrix(
        Array(0f, 3f, 0f, 2f),
        Array(3f, 0f, -27f, 0f),
        Array(0f, -27,  0, 174),
        Array(2f,  0,  174,  0)
      )
      h = concat(h, fliplr(clipLastColumn(h)))
      h = stack(h, flipud(clipLastRow(h))) / 512
      h(3, 3) = center
      h

    case 4 =>
      var h = Matrix(
        Array(0f,   -5,     0,   -3,    0    ),
        Array(-5f,    0,    52,    0,    34   ),
        Array(0f,    52,    0,   -276,  0    ),
        Array(-3f,    0,   -276,   0,   1454  ),
        Array(0f,   34,    0,   1454,  0   )
      ) / math.pow(2, 12).toFloat
      h = concat(h, fliplr(clipLastColumn(h)))
      h = stack(h, flipud(clipLastRow(h)))
      h(4, 4) = center
      h

    case 5 =>
      var h = Matrix(
        Array(0f,    35,    0,    20,    0,    18),
        Array(35f,   0,   -425,   0,   -250,    0),
        Array(0f,   -425,   0,   2500,   0,    1610),
        Array(20f,    0,    2500,  0,  -10200,   0),
        Array(0f,   -250,    0, -10200,  0,   47780),
        Array(18f,    0,    1610,  0,   47780,   0)
      ) / math.pow(2, 17).toFloat
      h = concat(h, fliplr(clipLastColumn(h)))
      h = stack(h, flipud(clipLastRow(h)))
      h(5, 5) = center
      h

    case 6 =>
      var h = Matrix(
        Array(0f,    -63,    0,    -35,    0,    -30,    0   ),
        Array(-63f,    0,    882,    0,    495,    0,    444  ),
        Array(0f,    882,    0,   -5910,   0,   -3420,   0   ),
        Array(-35f,    0,   -5910,   0,   25875,   0,   16460 ),
        Array(0f,    495,    0,    25875,  0,   -89730,  0   ),
        Array(-30f,    0,   -3420,   0,  -89730,   0,   389112),
        Array(0f,    44,     0,    16460,  0,   389112,  0   )
      ) / math.pow(2, 20).toFloat
      h = concat(h, fliplr(clipLastColumn(h)))
      h = stack(h, flipud(clipLastRow(h)))
      h(6, 6) = center
      h

    case 7 =>
      var h = Matrix(
        Array(0f,    231,     0,     126,     0,      105,      0,      100 ),
        Array(231f,    0,    -3675,    0,    -2009,     0,     -1715,     0    ),
        Array(0f,   -3675,    0,    27930,    0,     15435,     0,     13804  ),
        Array(126f,    0,     27930,   0,   -136514,    0,    -77910,     0    ),
        Array(0f,   -2009,    0,   -136514,   0,     495145,    0,     311780 ),
        Array(105f,    0,     15435,   0,    495145,    0,    -1535709,   0    ),
        Array(0f,   -1715,    0,   -77910,    0,    -1535709,   0,    6305740 ),
        Array(100f,    0,    13804,    0,    311780,    0,    6305740,    0    )
      ) / math.pow(2, 24).toFloat
      h = concat(h, fliplr(clipLastColumn(h)))
      h = stack(h, flipud(clipLastRow(h)))
      h(7, 7) = center
      h
    }
  }
}
