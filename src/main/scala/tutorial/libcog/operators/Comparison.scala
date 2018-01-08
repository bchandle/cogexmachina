/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package tutorial.libcog.operators

import java.nio.file.Paths

import cogdebugger._
import cogio.imagefiles.GrayscaleImage
import libcog._


/**
  * A simple example demonstrating comparison operators
  * @author Matthew Pickett
  */
object Comparison extends CogDebuggerApp(
  new ComputeGraph{
    val img = GrayscaleImage(Paths.get("src", "main", "resources", "oranges.jpg").toString)

    val noise = ScalarField.random(img.rows, img.columns)

    //compare each field point with an indentically shaped field
    val aboveNoise = img > noise
    //compare each field point with a scalar
    val belowHalf = img <= 0.5f
    //see if any field points are identical (extrememely unlikely)
    val equalityTest = img === noise
    val inverseEqualityTest = img !=== noise

    probeAll
  }
)

