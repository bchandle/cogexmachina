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

package cogx.runtime.execution

import akka.actor.{Props, Actor}
import cogx.platform.opencl._
import cogx.runtime.allocation.circuit.{OutputProxyKernel, InputProxyKernel}
import cogx.runtime.resources.GPU

/** Actor which supervises computation of a kernel circuit on a GPU.
  *
  * @param device OpenCL GPU managed by this supervisor.
  * @param gpu An object that allows late binding of the orderedKernels sequence (for multinode)
  * @param profileSize How often to print out profiling statistics (0 == never)
  *
  * @author Greg Snider
  */
private[runtime]
class GPUSupervisorActor(device: OpenCLDevice, gpu: GPU, profileSize: Int)
  extends Actor
{
  import SupervisorMessages._

  /** Create the non-actor GPUSupervisor that performs all the work. */
  val gpuSupervisor = new GPUSupervisor(device, gpu, profileSize)

  /** CPU kernels that need actors for their execution. */
  private val cpuActorKernels: Seq[OpenCLCpuKernel] = {
    val cpuKernels: Seq[OpenCLCpuKernel] =
      gpu.orderedKernels.filter(x => x.isInstanceOf[OpenCLCpuKernel]).
        map(_.asInstanceOf[OpenCLCpuKernel])
    cpuKernels.filter(_.hasActor)
  }

  /** Responses to messages. */
  def receive = {
    case Step =>
      try {
        gpuSupervisor.evaluate()
        sender ! StepDone(None)
      } catch {
        case e: Exception => sender ! StepDone(Some(e))
      }

    case Reset =>
      try {
        gpuSupervisor.reset()
        sender ! ResetDone(None)
      }
      catch {
        case e: Exception => sender ! ResetDone(Some(e))
      }

    case ProbeField(id) =>
      val memory = gpuSupervisor.readField(id)
      sender ! ProbeData(id, memory)

    ///////////////////////////////////////////////////////////////////////////
    // GPU Supervisors don't need to handle the FieldData message. In fact,
    // as implemented, they can't handle it. The problem is that while the
    // the supervisor is busy evaluating its circuit (in response to a Step
    // message), it cannot respond to or forward the FieldData messages
    // generated by its proxies.
    //
    // We could spawn a child actor to manage the proxies, but there's an
    // easier and cleaner way: let the ComputeNodeSupervisor (this supervisor's
    // parent) handle it. It should never be the case that an output proxy
    // managed by this supervisor is linked to an input proxy also managed by
    // this supervisor - that's a partitioner error. So, any FieldData messages
    // were bound to bubble up to the ComputeNodeSupervisor anyway, and, in the
    // time between the ComputeNodeSupervisor sending down a Step message and
    // receiving a StepDone reply, it's free to handle FieldData messages from
    // proxies.
    ///////////////////////////////////////////////////////////////////////////

    case DebugPrint =>
      println("+++ GPUSupervisorActor: DebugPrint")

    case x =>
      throw new Exception("unexpected message: " + x)
  }

  /** Pre start initialization and allocation. */
  override def preStart() {
    // Allocate actors for CPU kernels that need them.
    for (kernel <- cpuActorKernels) {
      kernel match {
        case ipk: InputProxyKernel  => // Handled by supervisor
        case opk: OutputProxyKernel => // Handled by supervisor
        case _ =>
          val actorRef = CogActorSystem.createCpuKernelActor(context,Props(new CPUKernelActor(kernel, self)),
            "CPUKernelActor_kernel_" + kernel.id)
          kernel.injectActor(actorRef)
      }
    }
    gpuSupervisor.preStart()
  }

  /** Post stop deallocation. */
  override def postStop() {
    gpuSupervisor.postStop()
  }
}