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

/** Convert an RGB color field to an HSV vector field.
  *
  * @author Ben Chandler
  */
object RGBtoHSV {
  def apply(input: VectorField): VectorField = {
    require(input.tensorShape == Shape(3))

    val R = input.vectorElement(0)
    val G = input.vectorElement(1)
    val B = input.vectorElement(2)
    val M = R.max(G).max(B)
    val m = R.min(G).min(B)
    val C = M - m
    val alpha = 0.5f * (2*R - G - B)
    val beta = 0.866025f * (G - B)
    val H = beta.atan2(alpha)
    val V = M
    val S = (C / (V + 0.0001f)) * (C > 0f)
    vectorField(H, S, V)
  }
}

object HSVtoRGB {
  def apply(input: VectorField): VectorField = {
    require(input.tensorShape == Shape(3))

    val scale = 0.9999f
    val offset = 0.0001f

    val H = input.vectorElement(0) max -math.Pi.toFloat*scale  min math.Pi.toFloat*scale
    val S = input.vectorElement(1) max offset min scale
    val V = input.vectorElement(2) max offset min scale

    val hNorm = H / (math.Pi/3f).toFloat
    val i = hNorm.floor
    val f = hNorm - i
    val p = V * (1-S)
    val q = V * (1-S*f)
    val t = V * (1-S*(1-f))
    val c0 = i === 0f
    val c1 = i === 1f
    val c2 = i === 2f
    val c3 = i === 3f
    val c4 = i === 4f
    val c5 = i === 5f

    val R = V*c0 + q*c1 + p*c2 + p*c3 + t*c4 + V*c5
    val G = t*c0 + V*c1 + V*c2 + q*c3 + p*c4 + p*c5
    val B = p*c0 + p*c1 + t*c2 + V*c3 + V*c4 + q*c5
    vectorField(R, G, B)
  }

}
