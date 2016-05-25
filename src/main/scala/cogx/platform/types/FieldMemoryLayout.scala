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

package cogx.platform.types

import cogx.platform.constraints.FieldLimits
import cogx.platform.types.ElementTypes._

/** Memory layout for a tensor field, optimized for efficient access on a GPU.
  * This also provides a uniform representation of fields that's
  * consistent on both CPU and GPU.
  *
  * Update: Padding of columns has been removed!  Motivation:
  *
  * 1. With caches now common in GPUs, this had no measurable performance benefit.
  * 2. The native runtime exposes the layout to the user apps, and no notion of
  *    padding was described.
  * 3. The padding introduced corner-cases where the footprint of certain fields
  *    grows dramatically, which affects buffer transfer times between GPUs and
  *    makes the GPU caches less effective.
  *
  * '''Scalar Fields'''
  *
  * Scalar fields are the easiest place to start. It looks like this:
  * {{{
  *
  *   2D Scalar field:
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page
  *   |                        |
  *   +------------------------+
  *        Memory (10 x 19)
  *
  * }}}
  * The array is then flattened to a linear array by reading the rows in order
  * from top to bottom. In general, this means that last index into the field
  * varies the fastest in the linear array. This linear array is defined to
  * be a "page."
  *
  * '''Vector Fields'''
  *
  * Tensor fields with tensor order > 0 (e.g. vector fields, matrix fields)
  * are laid out as several consecutive "pages," where each page contains one
  * element of that tensor at each point in the field. For example, a 2D vector
  * field with length 2 vectors would look like this:
  * {{{
  *
  *   2D Vector field:
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (vector element 0)
  *   |                        |
  *   +------------------------+
  *        Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (vector element 1)
  *   |                        |
  *   +------------------------+
  *        Memory (10 x 19)
  *
  * }}}
  *
  * '''Matrix Fields'''
  *
  * Matrix fields are laid out in row-major order:
  * {{{
  *
  *   2D matrix field:
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 0, 0)
  *   |                        |
  *   +------------------------+
  *        Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 0, 1)
  *   |                        |
  *   +------------------------+
  *        Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 1, 0)
  *   |                        |
  *   +------------------------+
  *        Memory (10 x 19)
  *
  *   +------------------------+
  *   |                        |
  *   |                        |
  *   |     Field (10 x 19)    |
  *   |                        |         <-- page (matrix element 1, 1)
  *   |                        |
  *   +------------------------+
  *        Memory (10 x 19)
  **
  * }}}
  *
  * '''Complex Fields'''
  *
  * Complex fields are organized as two real fields (one real, one imaginary)
  * concatentated in memory to form a single field. For example, a complex
  * tensor field would be laid out like this:
  *
  * {{{
  *   +----------------------------+
  *   |                            |
  *   |       tensor field         |
  *   |       (real part)          |
  *   |                            |
  *   +----------------------------+
  *
  *   +----------------------------+
  *   |                            |
  *   |       tensor field         |
  *   |       (imaginary part)     |
  *   |                            |
  *   +----------------------------+
  * }}}
  *
  *
  * '''Constraints'''
   *
  * Tensor fields have a maximum dimension and tensor order which is enforced
  * by this interface. Reading and writing this container requires that the
  * caller know both the field dimension and tensor order in order to call the
  * proper `read` or `write` method. Before reading and writing, the field
  * is initialized with zeroes.
  *
  * This is written assuming fields contain Floats, but could easily be
  * rewritten to contain any primitive type by using the Scala "specialized"
  * directive. The signature would need to change to something like
  * {{{
  *    class FieldMemoryLayout[@specialized(Int, Float, Double) T] (...)
  * }}}
  * then replacing "Float" references in the code to "T". This can't be done,
  * though, until the Vector and Matrix classes have been specialized.
  *
  * @author Greg Snider
  */
private[cogx]
class FieldMemoryLayout(val fieldType: FieldType)
    extends FieldLimits
    with FieldParameters
{
  /** The type of scalar that populates the tensor. */
  val elementType = fieldType.elementType

  val PixelSize = 4
  /** Number of "numbers" in each tensor in the field. */
  val numbersInTensor = elementType match {
    case Float32 =>
      // One number for each element of the tensor.
      tensorShape.points
    case Complex32 =>
      // Each complex scalar has two numbers, real and imaginary.
      tensorShape.points * 2
    case Uint8Pixel =>
      /** An image has 4 numbers: RGBA. */
      PixelSize
    case x =>
      require(requirement = false, "element type not supported yet: " + elementType)
      0
  }

  /** Number of bytes in a single-precision floating point number. */
  private val BytesPerFloat = 4

  /** Number of bytes in each "number" of the element. */
  val bytesPerNumber = elementType match {
    case Float32 =>
      // One number for each element of the tensor.
      BytesPerFloat
    case Complex32 =>
      // Each complex scalar has two numbers, real and imaginary, but each number is a float.
      BytesPerFloat
    case Uint8Pixel =>
      /** An image has 4 numbers: RGBA, but each number is a byte */
      1
    case x =>
      require(requirement = false, "element type not supported yet: " + elementType)
      0
  }

  require(dimensions <= MaxFieldDimensions, "too many dimensions: " + dimensions)
  require(tensorOrder <= MaxTensorOrder, "tensor order too large: " + tensorOrder)

  /** Columns rounded up for memory alignment when necessary. Note that images
    * do not have alignment requirements.
    *
    * Update: Padding of columns has been removed!  Motivation:
    *
    * 1. With caches now common in GPUs, this had no measurable performance benefit.
    * 2. The native runtime exposes the layout to the user apps, and no notion of
    *    padding was described.
    * 3. The padding introduced corner-cases where the footprint of certain fields
    *    grows dramatically, which affects buffer transfer times between GPUs and
    *    makes the GPU caches less effective.
    */
  val paddedColumns =
    elementType match {
      case Uint8Pixel =>
        columns
      case x =>
        dimensions match {
          case 0 => 1
          case _ => columns
        }
    }

  /** Number of elements required to represent a single tensor element in a
    * field, including whatever padding is needed for I/O efficiency.
    */
  val pageSize = layers * rows * paddedColumns

  /** Size of the buffer needed to hold the entire field, in units of "numbers".
    * A "number" can vary among types, e.g. Byte, Float, ....
    */
  private[cogx] val bufferSize = pageSize * numbersInTensor

  /** Size of the buffer needed to hold the entire field, in units of bytes.
    */
  private[cogx] val bufferSizeBytes = pageSize * numbersInTensor * bytesPerNumber

  /** Stride from beginning of one row to the next. */
  val fieldRowStride = paddedColumns

  /** Stride from beginning of one tensor element to the next. */
  val tensorStride = pageSize

  /** Stride from beginning of one part of a multi-component tensor element
    * (e.g. a complex) to the next part. */
  val partStride = pageSize * tensorShape.points
}


