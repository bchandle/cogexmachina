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

package toolkit.statistics

import org.apache.commons.math3.special.Erf

/** Case classes and helper function for picking randomly from different types
  * of distributions
  * @author Matthew Pickett
  */
sealed abstract class DistributionType() extends Serializable
/*pick a number between 0 and 1, multiply by scale, subtract offset*/
case class UniformDistribution(scale:Float, offset:Float) extends DistributionType
/*pick from a normal distribution parameterized by mu and sigma*/
case class NormalDistribution(mu:Float, sigma:Float) extends DistributionType

object distFunc{
  /** Transform a randomly picked float from uniform[0,1] to another distribution
    * @param uniform a float picked from a uniform distribution spanning 0 to 1
    * @param dist the target distribution type
    */
  def apply(uniform:Float, dist:DistributionType):Float =
    dist match{
      case UniformDistribution(scale,offset) => uniform*scale - offset
      case NormalDistribution(mu, sigma) =>
        //a small bias of 1e-9f is added to the uniform distribution to prevent
        //  NaNs when uniform is zero.
        (mu - math.sqrt(2.0)*sigma*Erf.erfcInv(2.0*uniform+1e-9f)).toFloat
    }
}