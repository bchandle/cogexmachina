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

/** A basis of 9 x 9 Gabor filters from the paper "The design and use of
  * steerable filters," Freeman and Adelson, 1991.
  *
  * These use the separable G2 and H2 filters, 7 different filters
  * total.
  *
  * @author Greg Snider
  */
object GaborBasisFilters {

  // Table IV, function f1 in the Freeman and Adelson paper.
  private val gf1 =
    new Vector(0.0094f, 0.1148f, 0.3964f, -0.0601f, -0.9213f,
      -0.0601f, 0.3964f, 0.1148f, 0.0094f)

  // Table IV, function f2 in the Freeman and Adelson paper.
  private val gf2 =
    new Vector(0.0008f, 0.0176f, 0.1660f, 0.6383f, 1.0000f,
      0.6383f, 0.1660f, 0.0176f, 0.0008f)

  // Table IV, function f3 in the Freeman and Adelson paper.
  private val gf3 =
    new Vector(0.0028f, 0.0480f, 0.3020f, 0.5806f, 0.0000f,
      -0.5806f, -0.3020f, -0.0480f, -0.0028f)

  // Table VI, function f1 in the Freeman and Adelson paper.
  private val hf1 =
    new Vector(0.0098f, 0.0618f, -0.0998f, -0.7551f, 0.000f,
      0.7551f, 0.0998f, -0.0618f, -0.0098f)

  // Table VI, function f2 in the Freeman and Adelson paper.
  private val hf2 =
    new Vector(0.0008f, 0.0176f, 0.1660f, 0.6383f, 1.0f,
      0.6383f, 0.1660f, 0.0176f, 0.0008f)

  // Table VI, function f3 in the Freeman and Adelson paper.
  private val hf3 =
    new Vector(0.0020f, 0.0354f, 0.2225f, 0.4277f, 0.0000f,
      -0.4277f, -0.2225f, -0.0354f, -0.0020f)

  // Table VI, function f4 in the Freeman and Adelson paper.
  private val hf4 =
    new Vector(0.0048f, 0.0566f, 0.1695f, -0.1889f, -0.7349f,
      -0.1889f, 0.1685f, 0.0566f, 0.0048f)

  // G basis filters, from Table IV
  private val G2a = gf1 outerProduct gf2
  private val G2b = gf3 outerProduct gf3
  private val G2c = gf2 outerProduct gf1

  // H basis filters, from Table VI
  private val H2a = hf1 outerProduct hf2
  private val H2b = hf4 outerProduct hf3
  private val H2c = hf3 outerProduct hf4
  private val H2d = hf2 outerProduct hf1

  /** The filters as matrices. */
  val filters = Array(G2a, G2b, G2c, H2a, H2b, H2c, H2d)

  /** The filters as non-negative matrices. */
  private val nonnegativeFilters =
    Array(G2a, G2b, G2c, H2a, H2b, H2c, H2d, -G2a, -G2b, -G2c, -H2a, -H2b, -H2c, -H2d)

  /** Gabor basis filters in a matrix field. */
  def apply() = MatrixField(1, filters.length,(row, col) => filters(col))

  /** Non-negative Gabor basis filters in a matrix field. */
  def nonnegative =
    MatrixField(1, filters.length,(row, col) => nonnegativeFilters(col))

  /** Project a 2D scalar image through the Gabor filter bank.
    *
    * @param input 2D scalar image to be filtered.
    * @return Vector field; one layer of the field is the scalar response to
    *        of the input to one of the filters in the filter bank.
    */
  //def project(input: ScalarField): VectorField =
  //  input crossCorrelate filterBank

  /** Adjoint of `project`.
    *
    * Takes the output from the `project` operator and computes the
    * approximate reconstruction of the input from it by running it backwards
    * through the filter bank.
    *
    * @param output The vector field to be backProjected to an reconstructed
    *        input.
    * @return Reconstructed input.
    */
  //def backProject(output: VectorField): ScalarField =
  //  output convolve filterBank

  /** Project a 2D scalar image through a Gabor filter bank, getting non-
    * negative outputs from the filters.
    *
    * This doubles the number of filters (and hence doubles the length of the
    * vectors in the output vector) relative to `project`.
    *
    * @param input 2D scalar image to be filtered.
    * @return Vector field; one layer of the field is the scalar response to
    *        of the input to one of the filters in the filter bank.
    */
  //def projectNonnegative(input: ScalarField): VectorField =
  //  (input crossCorrelate nonnegativeFilterBank) max 0


  /** Adjoint of `projectNonnegative`.
    *
    * Takes the output from the `project` operator and computes the
    * approximate reconstruction of the input from it by running it backwards
    * through the filter bank.
    *
    * @param output The vector field to be backProjected to an reconstructed
    *        input.
    * @return Reconstructed input.
    */
  //def backProjectNonnegative(output: VectorField): ScalarField =
  //  output convolve nonnegativeFilterBank
}
