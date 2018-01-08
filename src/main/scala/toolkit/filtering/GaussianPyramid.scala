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
package toolkit.filtering

import libcog._
import scala.collection.mutable.ArrayBuffer

/** Gaussian pyramid for an input image stream.
  *
  * @param input Input image stream.
  * @param smallestImage Smallest number of rows or columns for any image
  *        in the pyramid. This implicitly computes the number of levels in
  *        the pyramid.
  *
  * @author Greg Snider
  */
class GaussianPyramid(input: ScalarField, smallestImage: Int = 8) {
  /** Number of levels in the pyramid. */
  val levels = computeLevels(input.fieldShape)
  /** Images in the pyramid, level 0 being the highest resolution image. */
  val images = new Array[ScalarField](levels)
  /** Shape of each image in the pyramid. */
  val imageShape = new Array[Shape](levels)

  initialize()

  /** Initilialize the pyramid. */
  private def initialize() {
    // Lowpass filter design from
    // http://www.mathworks.com/help/vision/ref/vision.pyramidclass.html
    // (approximates a Gaussian for downsampling by factor of 2).
    val lowpass = new Vector(0.0625f, 0.25f, 0.375f, 0.25f, 0.0625f)
    val lowpassP = lowpass outerProduct lowpass
    val lowpassFilter = ScalarField(5, 5, lowpassP(_, _))
    images(0) = input
    for (i <- 1 until levels) {
      images(i) =
              images(i - 1).convolve(lowpassFilter, BorderClamp).downsample()
      imageShape(i) = images(i).fieldType.fieldShape
    }
  }

  /** Compute the number of levels in the pyramid. */
  private def computeLevels(imageShape: Shape): Int = {
    var imageSize = imageShape(0) min imageShape(1)
    var levels = 1
    while (imageSize > smallestImage) {
      levels += 1
      imageSize /= 2
    }
    levels
  }
}

/** Function object for creating a Gaussian pyramid.
 */
object GaussianPyramid {

  /** Create a Gaussian pyramid for an input stream.
    *
    * @param input The 2D input stream.
    * @return Pyramid: array of fields with coarsest (smallest) first.
    */
  def apply(input: Field, levels: Int): Array[Field] = {
    // Lowpass filter design from
    // http://www.mathworks.com/help/vision/ref/vision.pyramidclass.html
    // (approximates a Gaussian for downsampling by factor of 2).
    val lowpass = new Vector(0.0625f, 0.25f, 0.375f, 0.25f, 0.0625f)
    val lowpassP = lowpass outerProduct lowpass
    val lowpassFilter = ScalarField(5, 5, lowpassP(_, _))

    // Create the pyramid by lowpassing and subsampling

    val images = new ArrayBuffer[Field]
    images += input
    for (level <- 1 until levels) {
      val smaller = images.last.convolve(lowpassFilter, BorderZero).downsample()
        //samplingPolicy = DownsampleOutputConvolution(2))
      images += smaller
    }
    images.reverse.toArray
  }
}

