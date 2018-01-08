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
package toolkit.computervision.segmentation

import libcog._
import toolkit.solvers.LSQRSolver

/** Model the contents of a masked field as a Guassian distribution.
  *
  * This algorithm functions identically to GaussianSurfaceModel, but with
  * additional support for masks. Rather than computing the mean and
  * covariance of the entire field, MaskedGaussianSurfaceModel computes these
  * parameters using only information in the masked region. Probability density
  * is still estimated for the entire field.
  *
  * @author Ben Chandler
  */
object MaskedGaussianSurfaceModel {
  def apply(input: ScalarField, mask: ScalarField, temporalSmoothing: Float): ScalarField = {
    require(input.fieldShape.dimensions > 0)
    require(input.fieldShape == mask.fieldShape)
    require(temporalSmoothing >= 0f)
    require(temporalSmoothing < 1f)

    val points = mask.fieldReduceSum + 0.1f
    val instantMean = (input * mask).fieldReduceSum / points
    val instantStdDevSq = ((input - instantMean) * mask).sq.fieldReduceSum / points

    val (mean, stdDevSq) = temporalSmoothing match {
      case s if s == 0f => {
        (instantMean, instantStdDevSq)
      }

      case s if s > 0f => {
        val smoothMean = ScalarField(input.fieldShape)
        val smoothStdDevSq = ScalarField(input.fieldShape)

        smoothMean <== temporalSmoothing*smoothMean + (1f-temporalSmoothing)*instantMean
        smoothStdDevSq <== temporalSmoothing*smoothStdDevSq + (1f-temporalSmoothing)*instantStdDevSq

        (smoothMean, smoothStdDevSq)
      }
    }

    1f/(2f*3.14159f*stdDevSq).sqrt * (-1f * (input - mean).sq/(2f * stdDevSq)).exp
  }

  def apply(input: VectorField, mask: ScalarField, temporalSmoothing: Float = 0f): ScalarField = {
    require(input.fieldShape.dimensions > 0)
    require(input.fieldShape == mask.fieldShape)
    require(temporalSmoothing >= 0f)
    require(temporalSmoothing < 1f)

    val points = mask.fieldReduceSum + 0.1f
    val instantMean = (input * mask).fieldReduceSum / points
    val delta = matrixField((input - instantMean) * mask).transposeMatrices
    val instantCovariance = delta.transform(delta.transposeMatrices).fieldReduceSum / points

    val (mean, covariance) = temporalSmoothing match {
      case s if s == 0f => {
        (instantMean, instantCovariance)
      }

      case s if s > 0f => {
        val smoothMean = VectorField(Shape(), instantMean.tensorShape)
        val smoothCovariance = MatrixField(Shape(), instantCovariance.tensorShape)

        smoothMean <== temporalSmoothing*smoothMean + (1f-temporalSmoothing)*instantMean
        smoothCovariance <== temporalSmoothing*smoothCovariance + (1f-temporalSmoothing)*instantCovariance

        (smoothMean, smoothCovariance)
      }
    }

    val lsqr = LSQRSolver(input - mean, covariance.transform(_: VectorField),
      covariance.transform(_: VectorField), VectorField(input.fieldShape, input.tensorShape), 5)
    val x = lsqr(0)
    val deltaT = matrixField(input - mean)

    (deltaT.transform(x).reduceSum * -0.5f).exp
  }

  def apply(input: ColorField, temporalSmoothing: Float): ScalarField = {
    apply(input.toVectorField, temporalSmoothing)
  }
}


