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

package cogx.compiler.codegenerator.opencl.hyperkernels

import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, HyperKernel}
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.parser.op.PolarToComplexOp
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Kernel that converts two real scalar fields specifying the magnitude and
  * phase of a vector (i.e. polar coordinates) to a single complex scalar field.
  *
  * @author Dick Carter
  *
  * @param in The two input virtual field registers driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class MagnitudePhaseToComplexHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: Opcode,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{

  val code = new StringBuilder
  code append "    float magnitude = read(@in0);\n"
  code append "    float phase     = read(@in1);\n"
  code append "    float real      = magnitude * native_cos(phase);\n"
  code append "    float imaginary = magnitude * native_sin(phase);\n"
  code append "    @out0 = (float2) (real, imaginary);\n"
  addCode(code.toString())
//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object MagnitudePhaseToComplexHyperKernel extends HyperHelper {

  /**
   * Create a kernel that converts two real scalar fields specifying the magnitude and
   * phase of a vector (i.e. polar coordinates) to a single complex scalar field.
   *
   * @param in The two input virtual field registers driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    require(in.length == 2)
    val in0Type = in(0).fieldType
    val in1Type = in(1).fieldType
    require(in0Type.elementType == Float32)
    require(in0Type == in1Type)

    val expectedResultType = toComplex(in0Type)

    require(resultType == expectedResultType)
    require(operation == PolarToComplexOp)
    val addressing = bestAddressMode(in, resultType)
    new MagnitudePhaseToComplexHyperKernel(in, operation, resultType, addressing)
  }
}
