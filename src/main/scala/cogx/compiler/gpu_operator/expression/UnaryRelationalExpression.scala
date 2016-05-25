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

package cogx.compiler.gpu_operator.expression

import cogx.compiler.gpu_operator.types.GPUType
import cogx.platform.types.ElementTypes.{Float64, Int32, Int64}
import cogx.compiler.parser.semantics.SemanticError

/** Expression which takes one floating point argument. The type of the
  * result is integral, but there are weird rules in play here:
  *
  * 1. If the argument is float, the result is int.
  * 2. If the argument is floatn, the result is intn.
  * 3. If the argument is double, the result is int.
  * 4. If the argument is doublen, the result is longn  !!!
  *
  * @param operator The operation performed to produce the expression.
  * @param arg The argument to the operation.
  * @return Result expression.
  */
private[gpu_operator]
class UnaryRelationalExpression(operator: Operator,
                                arg: GPUExpression)
        extends GPUExpression(operator, {
          val isDouble = arg.gpuType.elementType == Float64
          val isVector = arg.gpuType.elements > 1
          if (isVector && isDouble)
            GPUType(Int64, arg.gpuType.elements)
          else if (isVector && !isDouble)
            GPUType(Int32, arg.gpuType.elements)
          else
            GPUType(Int32, 1)
        },
        Array(arg)
        )
        with SemanticError
{
  check(arg.gpuType.isFloat, "requires floating point argument")
}
