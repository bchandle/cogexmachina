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

/** Model the contents of a field as a Guassian distribution.
  *
  * GaussianSurfaceModel computes the mean and covariance of the input,
  * optionally smoothing the model over time, and uses these values to evaluate
  * the probability density function over the field. High values in the output
  * indicate points that are highly consistent with the Guassian model. Values
  * close to zero indicate inconsistent points. This tends to pop out arbitrary
  * unusual objects on approximately homogeneous backgrounds.
  *
  * The temporal smoothing parameter controls the rate at which the model is
  * permitted to change. With a value of 0, no smoothing is performed and a new
  * model is computed for every frame. Larger values mix a percentage of the
  * model from the previous frame. The smoothing parameter must be less than 1.
  *
  * GaussianSurfaceModel supports both univariate and multivariate Gaussian
  * models. ScalarField input will result in a one-dimensional model.
  * VectorField input will produce a multivariate model. Note that the
  * multivariate probability density function is approximated using an LSQR
  * solver. This is more efficient, but also inexact. The multivariate version
  * of GaussianSurfaceModel is dramatically less efficient than the univariate
  * version. Use the univariate version whenever possible.
  *
  * @author Ben Chandler
  */
object GaussianSurfaceModel {
  def apply(input: ScalarField, temporalSmoothing: Float): ScalarField = {
    require(input.fieldShape.dimensions > 0)
    require(temporalSmoothing >= 0f)
    require(temporalSmoothing < 1f)

    val points = input.fieldShape.points
    val instantMean = input.fieldReduceSum/points
    val instantStdDevSq = (input - instantMean).sq.fieldReduceSum/points

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

  def apply(input: VectorField, temporalSmoothing: Float): ScalarField = {
    require(input.fieldShape.dimensions > 0)
    require(temporalSmoothing >= 0f)
    require(temporalSmoothing < 1f)

    val points = input.fieldShape.points
    val instantMean = input.fieldReduceSum/points
    val delta = matrixField(input - instantMean).transposeMatrices
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
    apply(implicitly[VectorField](input), temporalSmoothing)
  }
}


