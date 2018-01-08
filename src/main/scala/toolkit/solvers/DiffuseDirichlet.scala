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

/**
 * Implements diffusion with both insulating and Dirichlet boundary
 * constraints using a multigrid algorithm.
 */
object DiffuseDirichlet {
  // Smallest grid size worth diffusing.
  private val MinGridSize = 2

  // 3 x 3 diffusion kernel (averages N, S, E, W neighbors only)
  private val diffusionFilter = ScalarField(3, 3, (i, j) => {
    Seq(
      Seq(0f, 1f, 0f),
      Seq(1f, 0f, 1f),
      Seq(0f, 1f, 0f)
    )(i)(j)
  }) / 4f
  /*
  private val diffusionFilter = ScalarField(3, 3, (i, j) => {
    Seq(
      Seq(1f, 2f, 1f),
      Seq(2f, 0f, 2f),
      Seq(1f, 2f, 1f)
    )(i)(j)
  }) / 12f
  */

  // 2 x 2 neighborhood filter (self, E, SE, and S).
  // Remember: convolution flips the kernel left/right and top/bottom, which
  // is why the following matrix is flipped.
  private val neighborFilter = ScalarField(3, 3, (i, j) => {
    Seq(
      Seq(1f, 1f, 0f),
      Seq(1f, 1f, 0f),
      Seq(0f, 0f, 0f)
    )(i)(j)
  })

  /**
    * Diffuse from "sources" in "image" until diffusion stops at insulating
    * "boundaries".
    *
    * @param image The image to be diffused.
    * @param sources The pixels in the image which are to be considered
    *        Dirichlet boundaries or sources that do not change as a result of
    *        the diffusion. A non-zero scalar in the field => "source", while a
    *        zero scalar => pixel that can be diffused through. This field
    *        essentially samples "image" to define the Dirichlet boundary
    *        constraints. THIS SHOULD BE A BINARY FIELD, 0's and 1's.
    * @param iterationCap The maximum number of diffusion iterations to run per
    *        level in the image pyramid.
    * @return The diffused version of "image" subject to the constraints
    *        in "sources" and "boundaries"
    */
  def apply(image: Field, sources: ScalarField, iterationCap: Int = 40): Field = {
    require(image.fieldShape == sources.fieldShape)
    require(image.fieldShape.dimensions == 2)
    require(iterationCap > 0)
    require(iterationCap <= 100)
    //sources.probe("sources " + image.fieldShape(0))

    val result = diffuse(image, sources, 5, iterationCap)
    // Do final merge of result and image
    image * sources + result * (1 - sources)
  }

  /** Recursively diffuse from the Dirichlet boundaries.
    *
    * @param image The image to be diffused.
    * @param sources A binary field showing boundaries as 1.0, others as 0.0.
    * @param iterations Number of diffusion iterations.
    * @param cap
    */
  private def diffuse(image: Field, sources: ScalarField, iterations: Int, cap: Int): Field = {
    val Tiny = 0.000001f  // To prevent division by 0
    if (image.fieldShape(0) <= MinGridSize && image.fieldShape(1) <= MinGridSize)
      image
    else {
      // Count the number of sources in each 2x2 subfield. `subsources` is this
      // count, half the size in each dimension of `sources`.
      val subSources =
        (sources.convolve(neighborFilter, BorderClamp)).downsample()

      // Mark each pixel in subsampled image as either constrained (the 2x2
      // subfield in the parent field corresponding to this pixel has at least
      // one source. Unconstrained means none of the pixels in the parent is
      // a source.
      val constrained = (subSources >= 0.9f)

      // Create a subsampled image. For each pixel: if constrained, we want the
      // average of the source pixels in the parent; if unconstrained, just use
      // 0.
      val subImage = constrained * (image.convolve(neighborFilter, BorderClamp).downsample() /
              (subSources + Tiny))

      // Recursion. Each time we go down a level, we increase the number
      // of iterations.
      val subSmooth = diffuse(subImage, constrained, 5 * iterations, cap)

      // Repeately diffuse using nearest neighbors, clamping sources after
      // each iteration
      var smooth = boundarySupersample(subSmooth, image, sources)
      for (i <- 0 until (iterations min cap)) {
        val diffused = smooth.convolve(diffusionFilter, BorderClamp)
        smooth = diffused * (1 - sources) + smooth * sources
      }
      smooth
    }
  }

  /** Upsample but only to targets that aren't source pixels.
    *
    * @param smallImage The image to be selectively upsampled to pixels which
    *        are not sources in the larger image.
    * @param biggerImage The image containing the pixel values for pixels
    *        where `sources` (denoting a Dirichlet boundary) are true.
    * @param biggerSources The Dirichlet boundary in `biggerImage`.
    */
  private def boundarySupersample(smallImage: Field,
                                  biggerImage: Field, 
                                  biggerSources: ScalarField): Field =
  {
    val up = smallImage.supersample.trim(biggerSources.fieldShape)
    val constrained = biggerSources
    val unconstrained = 1f - biggerSources
    // Blend unconstrained upsampled with constrainted original.
    val boundaryUpsample = (up * unconstrained) + (biggerImage * constrained)
    boundaryUpsample
  }
}
