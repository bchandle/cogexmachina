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

package toolkit.solvers

import libcog._
import toolkit.filtering.spatial.{BilinearFilter, LaplacianFilter}

/**
  * Poisson solver as described in "Real-Time Gradient-Domain Painting" by
  * McCann and Pollard (2008). This implementation covers only the solver, not
  * differentiation or blending modes. The expected input is a ColorFieldExpr
  * containing the Laplacian of the desired output.
  *
  * @author Ben Chandler
  */
object Poisson {
  def apply(input: VectorField, iterations: Int = 10): VectorField = {
    var working = (input * 0f, input, input): (VectorField, VectorField, VectorField)
    for (i <- 0 until iterations) {
      working = poissonIterate(working._1, working._2, working._3)
    }
    working._1
  }

  private def poissonIterate(f: VectorField, rhs: VectorField, residual: VectorField): (VectorField, VectorField, VectorField) = {
    val f1 = vcycle(residual, 1)
    val f2 = f1 + f
    val f3 = recenter(f2)
    val rhs1 = rhs - f3.convolve(LaplacianFilter(), BorderClamp)
    (f3, rhs, rhs1 * 0.5f)
  }

  private def vcycle(f: VectorField, h: Int): VectorField = {
    if (f.fieldShape == Shape(1, 1)) {
      f * 0f
    } else {
      val f2h = downsample(f)
      val u2h = vcycle(f2h, 2*h)
      val uh = upsample(u2h, Some(f))
      val uh1 = relax(uh, f, x0(h), h)
      val uh2 = relax(uh1, f, x1(h), h)
      uh2
    }
  }

  private def downsample(input: VectorField): VectorField = {
    input.convolve(BilinearFilter(), BorderZero).downsample()
  }

  private def upsample(input: VectorField, target: Option[VectorField] = None): VectorField = {
    target match {
      case Some(f) => input.upsample().trim(f.fieldShape).convolve(BilinearFilter(), BorderZero)
      case None => input.upsample().convolve(BilinearFilter(), BorderZero)
    }
  }

  private def relax(u: VectorField, f: VectorField, x: Float, h: Int): VectorField = {
    (f - u.convolve(lx(h, x), BorderClamp)) * (1/(m(h) - x))
  }

  private def x0(hI: Int): Float = {
    val h = hI.toFloat
    -2.1532f+1.5070f/h+0.5882f/(h*h)
  }

  private def x1(hI: Int): Float = {
    val h = hI.toFloat
    0.1138f+0.9529f/h+1.5065f/(h*h)
  }

  private def lx(hI: Int, x: Float): ScalarField = {
    val h = hI.toFloat
    val e = 1f/(3f*h*h) * (h*h+2f)
    val c = 1f/(3f*h*h) * (h*h-1f)

    val k = Array(
      Array(c, e, c),
      Array(e, x, e),
      Array(c, e, c)
    )

    ScalarField(3, 3, (r,c) => k(r)(c))
  }

  private def m(hI: Int): Float = {
    val h = hI.toFloat
    1f/(3f*h*h) * (-8f*h*h - 4f)
  }

  private def recenter(input: VectorField): VectorField = {
    input - average(input)
  }

  def average(input: VectorField): VectorField = {
    val k = Array(
      Array(0.25f, 0.25f, 0f),
      Array(0.25f, 0.25f, 0f),
      Array(0f, 0f, 0f)
    )

    val truncatedBox = ScalarField(3, 3, (r, c) => k(r)(c))

    if (input.fieldShape == Shape(1, 1)) {
      input
    } else {
      average(input.convolve(truncatedBox, BorderClamp).downsample()).supersample.trim(input.fieldShape)
    }
  }
}
