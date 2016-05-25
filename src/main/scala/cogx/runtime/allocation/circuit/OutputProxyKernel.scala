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

package cogx.runtime.allocation.circuit

import cogx.platform.types.{VirtualFieldRegister, AbstractKernel}
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLFieldRegister}
import cogx.compiler.parser.op._
import cogx.runtime.FieldID

/** An OutputProxyKernel acts much like an Actuator, but is actually linked,
  * under the covers, to one or more InputProxyKernels on other nodes.
  * This and the InputProxyKernel are used to break apart a circuit into
  * subcircuits; a signal crossing a node boundary must go out one node through
  * an OutputProxyKernel, and into the destination node(s) through an
  * InputProxyKernel.
  *
  * @author Greg Snider
  *
  * @param input     The virtual field register actually producing the data that is to be
  *                  sent across the network (likely a copy/clone of the
  *                  `proxyFor` argument)
  * @param proxyFor  The original virtual field register (from the unpartitioned circuit) whose
  *                  data must be proxied (i.e. is consumed by kernels in
  *                  multiple subcircuits after partitioning)
  */
private[cogx]
class OutputProxyKernel(input: VirtualFieldRegister, proxyFor: VirtualFieldRegister)
  extends OpenCLCpuSingleOutputKernel(OutputProxyOp, Array(input), proxyFor.fieldType, needActor=true) {

  require(input.fieldType == proxyFor.fieldType)


  val proxyForKernel = proxyFor.source
  val outputIdx = proxyFor.sourceOutputIndex match {
    case Some(i) => i
    case None => throw new RuntimeException("Compiler error: missing kernel output.")
  }

  /** Input and output proxy kernels save the id of the proxied node as it
    * existed in the original, unpartitioned circuit. This avoids having to
    * explicitly hook up input and output proxies to each other - the actor
    * supervisor hierarchy can find the appropriate field based on this ID. */
  val proxiedKernelId = FieldID(proxyForKernel.id, outputIdx)

  /** OutputProxyKernels need to indicate which InputProxyKernels they're
    * linked to so that their data can be routed intelligently.
    *
    * In the case of an actor supervisor hierarchy, these ids are used both
    * to forward field data messages down to the appropriate children and to
    * prevent propogating messages up to parent actors more than necessary. */
  var targetInputProxyIds: Seq[Int] = null

  outputs(0).name = {
    val s = proxyFor.name match {
      case "" => proxyForKernel.id
      case x  => x
    }
    "OutputProxy("+s+")"
  }

  /** Code which the user kernel must execute. */
  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister): Unit = {
    // No-op; all the work of proxying is delegated to an actor. We don't even
    // really need to allocate an output register for this kernel (but the
    // register allocation logic lives elsewhere).
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    * as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
    require(inputs.length == 0)
    new OutputProxyKernel(inputs(0), proxyFor)
  }

}