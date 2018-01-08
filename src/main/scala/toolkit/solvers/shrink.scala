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
package toolkit.solvers

import libcog._

/**
  * The ISTA shrinkage function sets values below lambda to zero and shrinks
  * values close to lambda with a multiplier < 1
  * @author Matthew Pickett
  */
object shrink {
  def apply(x:FieldArray, lambda:Field):FieldArray = {
    require(lambda.fieldShape equals Shape())
    require(lambda.tensorShape equals Shape())
    (1f-(lambda*(1f/x).abs).min(1f))*x
  }
  def apply(x:FieldArray, lambda:Float):FieldArray = apply(x, ScalarField(lambda))
  def apply(x:Field, lambda:Field):Field = apply(FieldArray(x), lambda)(0)
  def apply(x:Field, lambda:Float):Field = apply(FieldArray(x), ScalarField(lambda))(0)
}
