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
import toolkit.computervision.motion.OpticFlowPolynomial

trait Segmentation[T] {
  def flow: VectorField
  def smoothedFlow: VectorField
  def probability: ScalarField
  def croppedImage: T
  def estimate: ScalarField
}

/** Segment a video stream using motion information.
  *
  * @author Ben Chandler
  */
class MotionSegmentation(image: ScalarField, threshold: Float = 0.001f, newRate: Float = 0.25f) extends Segmentation[ScalarField] {
  require(image.fieldShape.dimensions == 2)
  require(image.fieldShape(0) > 30)
  require(image.fieldShape(1) > 30)
  require(newRate > 0f)
  require(newRate <= 1f)

  private val flowComputation = new OpticFlowPolynomial(image)

  private[toolkit] def cR = 10 until (image.fieldShape(0) - 10)
  private[toolkit] def cC = 10 until (image.fieldShape(1) - 10)

  private val flow1x = flowComputation.flow.vectorElement(0)(cR, cC)
  private val flow1y = flowComputation.flow.vectorElement(1)(cR, cC)
  private val avgX = flow1x.fieldReduceMedian
  private val avgY = flow1y.fieldReduceMedian

  private val croppedFlow = vectorField(flow1x - avgX, flow1y - avgY)

  private val rows = flow1x.fieldShape(0)
  private val cols = flow1x.fieldShape(1)

  private val (magnitude, outFlow) = newRate match {
    case r if r < 1f => {
      val temporalSmoothedFlow = VectorField(Shape(rows, cols), Shape(2))
      temporalSmoothedFlow <== r*croppedFlow + (1f-r)*temporalSmoothedFlow
      (temporalSmoothedFlow.dot(temporalSmoothedFlow).sqrt, temporalSmoothedFlow)
    }
    case r => {
      (croppedFlow.dot(croppedFlow).sqrt, croppedFlow)
    }
  }

  private val mean = magnitude.fieldReduceSum/(rows*cols)
  private val stdDevSq = (magnitude - mean).sq.fieldReduceSum / (rows*cols - 1f)
  private val prob = 1f/(2f*3.14159f*stdDevSq).sqrt * (-1f * (magnitude - mean).sq/(2f * stdDevSq)).exp

  def flow = croppedFlow
  def smoothedFlow = outFlow
  def probability = prob
  def croppedImage = image(cR, cC)
  def estimate = prob < threshold
}

class ColorMotionSegmentation(image: ColorField, threshold: Float = 0.001f, newRate: Float = 0.25f) extends Segmentation[ColorField] {
  private val seg = new MotionSegmentation(0.3f*image.red + 0.59f*image.green + 0.11f*image.blue, threshold, newRate)
  def flow = seg.flow
  def smoothedFlow = seg.smoothedFlow
  def probability = seg.probability
  def croppedImage = image(seg.cR, seg.cC)
  def estimate = seg.estimate
}


