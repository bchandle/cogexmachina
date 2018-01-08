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
package toolkit.computervision.motion

import libcog._
import toolkit.filtering.{GaussianPyramid, PolynomialExpansion}
import toolkit.filtering.spatial.{LanczosFilter, GaussianFilter}

/** Multi-scale optic flow using the algorithm described in "Two-frame motion
  * estimation based on polynomial expansion" by G. Farneback.
  *
  * This lacks a segmentation component, so boundaries are quite blurred. The
  * output is obtained from `flow`.
  *
  * @param image Input image stream to be analyzed.
  *
  * @author Greg Snider
  */
class OpticFlowPolynomial(image: ScalarField) {
  require(image.fieldShape.dimensions == 2, "Optic flow requires 2D stream")

  /** Local version of GaussianKernel that produces a Matrix.
    *
    * The standard library version of GaussianKernel is set up to produce
    * a ScalarField. PolynomialExpansion and PolynomialKernels can't work
    * with a Gaussian in this format, however. This local object is here
    * to simplify porting.
    *
    * -Ben
    */
  object LocalGaussianKernel {
    /**Estimate a "reasonable" odd size for kernel with "sigma". */
    def size(sigma: Float): Int = {
      val rough = (6 * sigma).ceil.toInt
      // Force size to be odd
      (rough / 2) * 2 + 1
    }

    /** Create an automatically-sized Gaussian kernel. */
    def apply(sigma: Float): Matrix = apply(size(sigma), sigma)

    /** Create a "width" x "width" Gaussian kernel with stdev "sigma". */
    def apply(width: Int, sigma: Float): Matrix = {
      require(width % 2 ==1, "kernel width must be odd")

      val center = width/2
      val sigmaSq = sigma*sigma
      val amplitude = 1.0f/(2*math.Pi*sigmaSq)
      val fcn:(Int, Int)=>Float = (r, c)=>{
        val distSq = (center-r)*(center-r)+(center-c)*(center-c)
        (amplitude*math.exp((-1.0f / (2 * sigmaSq)) * distSq)).toFloat
      }
      Matrix(width, width, fcn)
    }
  }

  val pyramid = new GaussianPyramid(image, 2)

  // Traverse the pyramid from coarsest to finest.
  private var d: VectorField = VectorField(pyramid.imageShape.last, Shape(2))
  for (level <- pyramid.levels - 1 to 0 by -1) {
    // Create polynomial basis set
    val Sigma = 1.5f
    val gaussian = LocalGaussianKernel(Sigma)
    val poly2 = new PolynomialExpansion(pyramid.images(level), gaussian)
    val poly2A = poly2.A
    val poly2b = poly2.b
    val poly1A = Field(poly2.A.fieldType)
    poly1A <== poly2.A
    val poly1b = Field(poly2.b.fieldType)
    poly1b <== poly2.b

    // Eqn (9, 10)
    val A = (poly1A + poly2A.warp(d)) / 2
    val db = (poly1b - poly2b.warp(d)) / 2

    // Eqn (12, 13)
    val smooth = GaussianFilter(13, 5f)
    val AA = (A transform A).convolve(smooth, BorderClamp)
    val Adb = (A transform db).convolve(smooth, BorderClamp)

    // Output flow field, called d in paper. Eqn (13)
    d = AA solve Adb

    // Upsample and interpolate for next iteration
    if (level > 0)
      d = d.upsample().trim(pyramid.images(level - 1).fieldShape).convolve(
              (LanczosFilter(2, 2) * 2), BorderClamp)
  }

  /** Output flow field. */
  val flow = d
}

