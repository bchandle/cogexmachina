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
import toolkit.filtering.frequency.FilterFFT

/** Solves the screened Poisson equation in the Fourier domain. See the paper
  * "Fourier analysis of the 2D screened Poisson equation for gradient
  * domain problems," by Bhat, Curless, Cohen and Zitnick, 2008.
  *
  * @author Greg Snider
  */
object ScreenedPoissonSolver extends Logarithm {

  /** Integrate a `gradient` field subject to the constraint of matching a
    * reference `image` field as closely as possible. Since this is
    * impossible in general, there is a tradeoff term, `lambda`, which allows
    * one to balance fidelity to the reference `image` (larger `lambda`) and
    * the `gradient` (smaller lambda).
    *
    * @param imageFFT The "periodized" FFT of the reference image field which we
    *        want to match as best we can. See the paper "Periodic plus
    *        Smooth Image Decomposition," Journal of Mathematical Imaging and
    *        Vision, vol 39:2, pp. 161-179, 2011, for a description of the
    *        periodic FFT.
    * @param gradient The desired gradient field to be integrated.
    * @param lambda Tradeoff between fidelity to `image` (larger value) and
    *        integrated `gradient` (smaller value).
    * @return Fourier transform of image which best satisfies the given
    *        constraints.
    */
  def apply(imageFFT: ComplexField,
            gradient: VectorField, lambda: Float): ComplexField = 
  {
    require(imageFFT.rows == imageFFT.columns, "image must be square")
    require(gradient.rows == gradient.columns, "gradient image must be square")
    require(imageFFT.rows == gradient.rows, "image and gradient must be same size")
    require(isPowerOf2(imageFFT.rows), "image must be power of 2 in size")

    val laplacian: ComplexMatrix = FilterFFT(imageFFT.fieldShape, Matrix(
      Array(0f,  1f,  0f),
      Array(1f, -4f,  1f),
      Array(0f,  1f,  0f)
    ))

    // We compensate for pole(s) in the transfer function by clipping thme
    val denominator = ComplexField(clipZeroes(laplacian * -1 + lambda))
    val numerator = lambda * imageFFT - gradient.backwardDivergence.fft
    numerator / denominator
  }

  /** Clip very small values to 1. (Zero clipping in the frequency domain). This
    * is used for filters which are in the denominator of a frequency function.
    *
    * @param matrix Filter in frequency domain.
    * @return Matrix with very small values set to 1.
    */
  private def clipZeroes(matrix: ComplexMatrix): ComplexMatrix =
   matrix.map(c => if (c.magnitude <= 0.00000001f) Complex(1f, 0) else c)
}
