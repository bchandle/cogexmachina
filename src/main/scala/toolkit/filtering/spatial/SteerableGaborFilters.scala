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

package toolkit.filtering.spatial

import libcog._

/** Steerable filters as described in the paper "The Design and Use of
  * Steerable Filters," Freeman and Adelson, IEEE Trans. Patt. Anal. and
  * Machine Intell., vol 13, no. 9, pp 891-901, Sept. 1991.</p>
  *
  * The filters follow the naming convention of the paper to make it easy
  * to cross reference the paper and implementation.
  *
  * I have chosen the *SEPARABLE* kernels for the basis kernels, so
  * these can all be implemented more efficiently than with the FFT.
  *
  * @author Greg Snider
  */

object SteerableGaborFilters {
  /** Convert filter size to an appropriate scaling for G2 filters. */
  private def g2Scale(filterSize: Int): Float = filterSize / 5f

  /**
   * Base class for steerable filters which translates between (row, col)
   * coordinates of matrices and the (x, y) coordinates used in the paper.
   * Note that kernels use an x, y coordinate system with (0, 0) at the center,
   * which is why we require steerable kernels to be of odd width and height.
   * Also note that we would like the y axis to increase upwards, but the
   * conventional image processing scheme increases downward. We correct for
   * that here by "flipping" the y coordinate about the x axis.
   *
   * @param size Width and height of kernel, in pixels.
   * @param scale The size of the basis functions.
   * @param pixel Supplies the value of each pixel given its (x, y) coordinates.
   */
  abstract class Base(size: Int, scale: Float, pixel: (Float, Float) => Float)
          extends Matrix(size, size)
  {
    require(size % 2 == 1)
    val offset = size / 2
    for (row <- 0 until rows; col <- 0 until columns) {
      val y = -(col - offset) / scale
      val x = (row - offset) / scale
      this(row, col) = pixel(x, y)
    }

    /** Steering coefficient for the filter. */
    def k(angle: Float): Float
  }

  /** Figure 16 in Freeman and Adelson paper. See object Steerable. */
  class G2a(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.9213f * (2 * x * x - 1) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float =
      (math.cos(angle) * math.cos(angle)).toFloat
  }

  /** Figure 16 in Freeman and Adelson paper. See object Steerable. */
  class G2b(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            1.843f * x * y * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float =
      (-2 * math.cos(angle) * math.sin(angle)).toFloat
  }

  /** Figure 16 in Freeman and Adelson paper. See object Steerable. */
  class G2c(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.9213f * (2 * y * y - 1) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float =
      (math.sin(angle) * math.sin(angle)).toFloat
  }

  /** Figure 18 in Freeman and Adelson paper. See object Steerable. */
  class G4a(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            1.2346f * (0.75f - 3 * x*x + x*x*x*x) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      cos * cos * cos * cos
    }
  }

  /** Figure 18 in Freeman and Adelson paper. See object Steerable. */
  class G4b(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            1.2346f * (-1.5f * x + x*x*x) * y * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      -4f * cos * cos * cos * sin
    }
  }

  /** Figure 18 in Freeman and Adelson paper. See object Steerable. */
  class G4c(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            1.2346f * (x*x - 0.5f) * (y*y - 0.5f) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      6f * cos * cos * sin * sin
    }
  }

  /** Figure 18 in Freeman and Adelson paper. See object Steerable. */
  class G4d(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            1.2346f * (-1.5f * y + y*y*y) * x * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      -4f * cos * sin * sin * sin
    }
  }


  /** Figure 18 in Freeman and Adelson paper. See object Steerable. */
  class G4e(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            1.2346f * (0.75f - 3 * y*y + y*y*y*y) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val sin = math.sin(angle).toFloat
      sin * sin * sin * sin
    }
  }

  /** Factory for creating a G2 filter steered to "angle".  */
  object G2 {
    def apply(size: Int, angle: Float): Matrix = {
      val g2a = new G2a(size)
      val g2b = new G2b(size)
      val g2c = new G2c(size)
      val sum = g2a * g2a.k(angle) + g2b * g2b.k(angle) + g2c * g2c.k(angle)
      val steeredKernel = new Matrix(size, size)
      for (row <- 0 until size; col <- 0 until size)
        steeredKernel(row, col) = sum(row, col)
      steeredKernel
    }
  }

  /** Figure 17 in Freeman and Adelson paper. See object Steerable. */
  class H2a(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.9780f * (-2.254f * x + x * x * x) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      cos * cos * cos
    }
  }

  /** Figure 17 in Freeman and Adelson paper. See object Steerable. */
  class H2b(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.9780f * (-0.7515f + x * x) * y * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      -3f * cos * cos * sin
    }
  }

  /** Figure 17 in Freeman and Adelson paper. See object Steerable. */
  class H2c(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.9780f * (-0.7515f + y * y) * x * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      3f * cos * sin * sin
    }
  }

  /** Figure 17 in Freeman and Adelson paper. See object Steerable. */
  class H2d(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.9780f * (-2.254f * y + y * y * y) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val sin = math.sin(angle).toFloat
      -sin * sin * sin
    }
  }

  /** Figure 19 in Freeman and Adelson paper. See object Steerable. */
  class H4a(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.3975f * (7.189f * x - 7.501f * x*x*x + x*x*x*x*x) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      cos * cos * cos * cos * cos
    }
  }

  /** Figure 19 in Freeman and Adelson paper. See object Steerable. */
  class H4b(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.3975f * (1.438f - 4.501f * x*x + x*x*x*x) * y * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      -5f * cos * cos * cos * cos * sin
    }
  }

  /** Figure 19 in Freeman and Adelson paper. See object Steerable. */
  class H4c(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.3975f * (x*x*x - 2.225f * x) *(y*y - 0.6638f) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      10 * cos * cos * cos * sin * sin
    }
  }

  /** Figure 19 in Freeman and Adelson paper. See object Steerable. */
  class H4d(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.3975f * (y*y*y - 2.225f * y) * (x*x - 0.6638f) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      -10 * cos * cos * sin * sin * sin
    }
  }

  /** Figure 19 in Freeman and Adelson paper. See object Steerable. */
  class H4e(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.3975f * (1.438f - 4.501f * y*y + y*y*y*y) * x * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat
      5 * cos * sin * sin * sin * sin
    }
  }

  /** Figure 19 in Freeman and Adelson paper. See object Steerable. */
  class H4f(size: Int)
          extends Base(size, g2Scale(size), (x: Float, y: Float) =>
            0.3975f * (7.189f * y - 7.501f * y*y*y + y*y*y*y*y) * math.exp(-(x * x + y * y)).toFloat)
  {
    def k(angle: Float): Float = {
      val sin = math.sin(angle).toFloat
      -sin * sin * sin * sin * sin
    }
  }

  /** Factory for creating an H2 filter steered to "angle".  */
  object H2 {
    def apply(size: Int, angle: Float): Matrix = {
      val h2a = new H2a(size)
      val h2b = new H2b(size)
      val h2c = new H2c(size)
      val h2d = new H2d(size)
      val sum = h2a * h2a.k(angle) + h2b * h2b.k(angle) + h2c * h2c.k(angle) +
              h2d * h2d.k(angle)
      val steeredKernel = new Matrix(size, size)
      for (row <- 0 until size; col <- 0 until size)
        steeredKernel(row, col) = sum(row, col)
      steeredKernel
    }
  }

  //------------------------ 2nd order kernels --------------------------------

  /** Factory which produces a G2a (real part) and H2a (imag part) kernel. */
  object GH2a {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G2a(size)) + new ComplexMatrix(new H2a(size)) * I
    }
  }

  /** Factory which produces a G2b (real part) and H2b (imag part) kernel. */
  object GH2b {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G2b(size)) + new ComplexMatrix(new H2b(size)) * I
    }
  }

  /** Factory which produces a G2c (real part) and H2c (imag part) kernel. */
  object GH2c {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G2c(size)) + new ComplexMatrix(new H2c(size)) * I
    }
  }

  /** Factory which produces zero (real part) and H2d (imag part) kernel. */
  object GH2d {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new H2d(size)) * I
    }
  }

  //------------------------ 4th order kernels --------------------------------

  /** Factory which produces a G4a (real part) and H4a (imag part) kernel. */
  object GH4a {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G4a(size)) + new ComplexMatrix(new H4a(size)) * I
    }
  }

  /** Factory which produces a G4b (real part) and H4b (imag part) kernel. */
  object GH4b {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G4b(size)) + new ComplexMatrix(new H4b(size)) * I
    }
  }

  /** Factory which produces a G4c (real part) and H4c (imag part) kernel. */
  object GH4c {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G4c(size)) + new ComplexMatrix(new H4c(size)) * I
    }
  }

  /** Factory which produces G4d (real part) and H4d (imag part) kernel. */
  object GH4d {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G4d(size)) + new ComplexMatrix(new H4d(size)) * I
    }
  }

  /** Factory which produces G4e (real part) and H4e (imag part) kernel. */
  object GH4e {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new G4e(size)) + new ComplexMatrix(new H4e(size)) * I
    }
  }

  /** Factory which produces 0 (real part) and H4f (imag part) kernel. */
  object GH4f {
    def apply(size: Int): ComplexMatrix = {
      new ComplexMatrix(new H4f(size)) * I
    }
  }

  //----------------- 4th order interpolation functions -----------------------
  import scala.math.{cos, sin, pow}

  def kg4a(theta: Float) = pow(cos(theta), 4).toFloat
  def kg4b(theta: Float) = (-4 * pow(cos(theta), 3) * sin(theta)).toFloat
  def kg4c(theta: Float) = (6 * pow(cos(theta), 2) * pow(sin(theta), 2)).toFloat
  def kg4d(theta: Float) = (-4 * cos(theta) * pow(sin(theta), 3)).toFloat
  def kg4e(theta: Float) = pow(sin(theta), 4).toFloat

  def kh4a(theta: Float) = pow(cos(theta), 5).toFloat
  def kh4b(theta: Float) = (-5 * pow(cos(theta), 4) * sin(theta)).toFloat
  def kh4c(theta: Float) = (10 * pow(cos(theta), 3) * pow(sin(theta), 2)).toFloat
  def kh4d(theta: Float) = (-10 * pow(cos(theta), 2) * pow(sin(theta), 3)).toFloat
  def kh4e(theta: Float) = (5 * cos(theta) * pow(sin(theta), 4)).toFloat
  def kh4f(theta: Float) = (-pow(sin(theta), 5)).toFloat


//  private def square(v: Float): Float = v * v

//  private def cube(v: Float): Float = v * v * v

//  private def power4(v: Float): Float = v * v * v * v
}
