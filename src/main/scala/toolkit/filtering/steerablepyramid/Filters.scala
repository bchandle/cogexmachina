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

package toolkit.filtering.steerablepyramid

import libcog._

/** Interface to steerable pyramid filter generator.
  *
  * Transform described  in:
  *
  * INPROCEEDINGS{Simoncelli95b,
  *	TITLE = "The Steerable Pyramid: A Flexible Architecture for
  *		 Multi-Scale Derivative Computation",
  * AUTHOR = "E P Simoncelli and W T Freeman",
  *	BOOKTITLE = "Second Int'l Conf on Image Processing",
  *	ADDRESS = "Washington, DC", MONTH = "October", YEAR = 1995 }
  *
  * Filter kernel design described in:
  *
  *INPROCEEDINGS{Karasaridis96,
  * TITLE = "A Filter Design Technique for
  *		Steerable Pyramid Image Transforms",
  * AUTHOR = "A Karasaridis and E P Simoncelli",
  *	BOOKTITLE = "ICASSP",	ADDRESS = "Atlanta, GA",
  *	MONTH = "May",	YEAR = 1996 }
  *
  * @author Greg Snider
  */

trait Filters {
  val harmonics: Array[Int]

  val lowpass0: ScalarField

  val lowpass: ScalarField

  val mtx: ScalarField

  val highpass0: ScalarField

  val bandpass: Array[ScalarField]

  /** Convert a matrix to a 2D scalar field. */
  protected def toScalarField(matrix: Matrix): ScalarField = {
    ScalarField(matrix.rows, matrix.columns, (row, col) => matrix(row, col))
  }
}
