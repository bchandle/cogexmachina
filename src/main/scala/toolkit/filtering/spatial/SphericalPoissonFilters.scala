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

/**
  * Implements the spherical Poisson filter as described in "Optical Flow
  * Estimation from Monogenic Phase" by M. Felsberg, section 3.4. Although
  * most naturally expressed using quaternions, we simply use scalars.
  *
  * @author Greg Snider
  */

class SphericalPoissonFilters(scale:Float){
  private val TruncationThreshold = 0.01f
  private val MaxFilterHalfSize = 100
  private val size = filterSize(scale)
  private val centerPos = size/2

  /** The even kernel. */
  private val evenFcn: (Int,Int)=>Float = (row, col) =>
      he(row, col, scale - 1) + 2 * he(row, col, scale) + he(row, col, scale + 1)
  val even = ScalarField(size,size,evenFcn)

  /** The first odd kernel. */
  private val odd1Fcn:(Int,Int)=>Float = (row, col) =>
    ho1(row, col, scale - 1) + 2 * ho1(row, col, scale) + ho1(row, col, scale + 1)
  val odd1 = ScalarField(size,size,odd1Fcn)

  /** The second odd kernel. */
  private val odd2Fcn:(Int,Int)=>Float = (row, col) =>
      ho2(row, col, scale - 1) + 2 * ho2(row, col, scale) + ho2(row, col, scale + 1)
  val odd2 = ScalarField(size,size,odd2Fcn)


  /**
   * Compute "optimal" filter size for a given "scale". The filters have
   * infinite extent and must be truncated; to do this we truncate when the
   * value away from the origin drops to TruncationThreshold * the center
   * value.
   */
  private def filterSize(scale: Float): Int = {
    val thresholdHe = he(1, 0, scale) * TruncationThreshold
    val thresholdHo1 = ho1(1, 0, scale) * TruncationThreshold
    val thresholdHo2 = ho2(0, 1, scale) * TruncationThreshold
    //var done = false
    for (i <- 0 until MaxFilterHalfSize) {
      println("he(" + i + ") = " + he(i, 0, scale-1) + " " + he(i, 0, scale) + " " + he(i, 0, scale+1))
      println("ho1(" + i + ") = " + ho1(i, 0, scale-1) + " " + ho1(i, 0, scale) + " " + ho1(i, 0, scale+1))
      println("ho2(" + i + ") = " + ho2(0, i, scale-1) + " " + ho2(0, i, scale) + " " + ho2(0, i, scale+1))
      val heOK = (he(i, 0, scale) < thresholdHe) &&
              (he(i, 0, scale - 1) < thresholdHe) &&
              (he(i, 0, scale + 1) < thresholdHe)
      val ho1OK = (ho1(i, 0, scale) < thresholdHo1) &&
              (ho1(i, 0, scale - 1) < thresholdHo1) &&
              (ho1(i, 0, scale + 1) < thresholdHo1)
      val ho2OK = (ho2(0, i, scale) < thresholdHo2) &&
              (ho2(0, i, scale - 1) < thresholdHo2) &&
              (ho2(0, i, scale + 1) < thresholdHo2)
      if (heOK && ho1OK && ho2OK)
        return 2 * i + 1
    }
    throw new RuntimeException("SphericalPoison filter size is insanely huge.")
  }

  /** Compute Poisson filter coefficent he at ("row", "col") centered. */
  private def he(row: Int, col: Int, scale: Float): Float =
    scale / denominator(row, col, scale)

  /** Compute Poisson filter coefficent he at ("row", "col") centered. */
  private def ho1(row: Int, col: Int, scale: Float): Float =
    (centerPos-row) / denominator(row, col, scale)

  /** Compute Poisson filter coefficent he at ("row", "col") centered. */
  private def ho2(row: Int, col: Int, scale: Float): Float =
    (centerPos-col) / denominator(row, col, scale)

  /** Denominator calculation for all filter components. */
  private def denominator(row: Int, col: Int, scale: Float): Float = {

    val centerDistSq = (row-centerPos)*(row-centerPos) + (col-centerPos)*(col-centerPos)
    (2 * math.Pi * math.pow(scale * scale + centerDistSq, 1.5)).toFloat
  }
}