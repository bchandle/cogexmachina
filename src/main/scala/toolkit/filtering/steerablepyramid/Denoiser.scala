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
/*
package user.greg.steerablepyramid

import cog._
import coggui.CogBrowserApp
import libcog.sensors.GrayscaleImage

/**
  * Implements the Denoiser described in "Design and Use of Steerable Filters,"
  * by Freeman and Adelson.
  *
  * @author Greg Snider
  */

class Denoiser(cleanInput: DynamicScalarField) {
  val Levels = 2
  val NoiseAmplitude = 0.2f
  val random = new java.util.Random
  val noise = ScalarField(cleanInput.rows, cleanInput.columns,
    (_, _) => (random.nextFloat - 0.5f) * NoiseAmplitude)
    //(_, _) => (random.nextGaussian.toFloat) * NoiseAmplitude)
  val input = DynamicScalarField((cleanInput + noise).max(0f).min(1f))
  val filters = FiltersOrder3
  val hilbertFilters = FiltersHilbertOrder3
  val low0 = DynamicScalarField(input convolve filters.lowpass0, "low 0")
  val high0 = DynamicScalarField(input convolve filters.highpass0, "high 0")
  //val restored = DynamicScalarField((filterBank(low0).convolve(filters.lowpass0) +
  //        high0.convolve(filters.highpass0)).max(0f).min(1f), "restored  ")
  val restored = DynamicScalarField(filterBank(low0).convolve(filters.lowpass0).
          max(0f).min(1f), "restored  ")

  def filterBank(in: ScalarFieldExpr, level: Int = 0): ScalarFieldExpr = {
    if (level >= Levels)
      in
    else {
      val L = DynamicScalarField((in convolve filters.lowpass).downsample(), "L" + level)
      val B = Array.tabulate[DynamicScalarField](filters.bandpass.length) {
        i => DynamicScalarField(in crossCorrelate ScalarField(filters.bandpass(i)), "B" + level + i)
      }
      val BHilbert = Array.tabulate[DynamicScalarField](filters.bandpass.length) {
        i => DynamicScalarField(in crossCorrelate ScalarField(hilbertFilters.bandpass(i)), "BH" + level + i)
      }
      val energy = Array.tabulate[DynamicScalarField](filters.bandpass.length) {
        i => DynamicScalarField(B(i) * B(i) + BHilbert(i) * BHilbert(i), "E" + level + i)
      }
      val cleaned = Array.tabulate[ScalarFieldExpr](filters.bandpass.length) {
        i => {
          DynamicScalarField(softThreshold(B(i), energy(i), level), "C" + level + i)
        }
      }

      // Reconstruction
      val rB = Array.tabulate[ScalarFieldExpr](filters.bandpass.length) {
        i => {
          val cleaned = softThreshold(B(i), energy(i), level)
          //DynamicScalarField(B(i) convolve ScalarField(filters.bandpass(i)), "rB" + level + i)
          DynamicScalarField(cleaned convolve ScalarField(filters.bandpass(i)), "rB" + level + i)
        }
      }
      val rL = filterBank(L, level + 1).upsample() convolve filters.lowpass
      DynamicScalarField(rL + rB.reduceLeft(_ + _), "reconstruction " + level)
    }
  }


  /** Eqn (17) */
  def softThreshold(coeff: ScalarFieldExpr, energy: ScalarFieldExpr, level: Int): ScalarFieldExpr = {
    val S = 0.5f
    val T = level match {
      case 0 => 0.08f
      case 1 => 0.10f
      case 2 => 0.9f
    }
    coeff / (1f + (-S * (energy.sqrt - T)).exp)
  }
}

/** Test code for the Denoiser. */
object TestDenoise extends CogBrowserApp("Denoiser") {
  val input = GrayscaleImage("releaseResources/images/cameraman.jpg")
  new Denoiser(input)
}*/
