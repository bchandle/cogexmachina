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

package toolkit.filtering.contourlets

import libcog._

/** The fundamental sampling matrices. See "Multidimension filter banks
  * and multiscale geometric representations," Do and Lu, Foundations and
  * Trends in Signal Processing, 2011.
  *
  * Note that the names here correspond to those in the Matlab code, not the
  * paper. Here's the mapping:
  * {{{
  *      paper       code (matlab and here)
  *
  *       Q0           Q1
  *       Q1           Q2
  *       R0           R1
  *       R1           R2
  *       R2           R3
  *       R3           R4
  *
  *       D1           D1
  *       D2           D2
  * }}}
  *
  *
  * the same as Q1 in the Matlab code).
  *
  * @author Greg Snider
  */
object SamplingMatrices {

  /** Quincunx subsampling matrix 0, eqn 3.3 */
  val Q1 = Matrix(
    Array(1f, -1f),
    Array(1f,  1f)
  )

  /** Quincunx subsampling matrix 1, eqn 3.4 */
  val Q2 = Matrix(
    Array( 1f, 1f),
    Array(-1f, 1f)
  )

  /** Shearing matrix 0, eqn 3.17 */
  val R1 = Matrix(
    Array( 1f, 1f),
    Array( 0f, 1f)
  )

  /** Shearing matrix 1, eqn 3.17 */
  val R2 = Matrix(
    Array( 1f,-1f),
    Array( 0f, 1f)
  )

  /** Shearing matrix 2, eqn 3.17 */
  val R3 = Matrix(
    Array( 1f, 0f),
    Array( 1f, 1f)
  )

  /** Shearing matrix 3, eqn 3.17 */
  val R4 = Matrix(
    Array( 1f, 0f),
    Array(-1f, 1f)
  )

  /** Dyadic row subsampling matrix 1, eqn 3.19 */
  val D1 = Matrix(
    Array(2f, 0f),
    Array(0f, 1f)
  )

  /** Dyadic column subsampling matrix 2, eqn 3.19 */
  val D2 = Matrix(
    Array( 1f, 0f),
    Array( 0f, 2f)
  )

  /** Conventional grid upsampling. */
  val I2 = Matrix(
    Array( 2f, 0f),
    Array( 0f, 2f)
  )

  // Derived sampling matrices, see ppdec.m
  val P1 = R1 * Q1
  val P2 = R2 * Q2
  val P3 = R3 * Q2
  val P4 = R4 * Q1
}
