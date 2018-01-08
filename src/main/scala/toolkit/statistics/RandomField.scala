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

import libcog._

/**
  * Set of constructors for intializing fields with random numbers with more
  * options and flexibility than the built in API. Mutators can later be applied
  * to these fields.
  *
  * Usage: RandomField(template) where template is a field with the shape you want
  *  to use
  * @author Matthew Pickett
  */


object RandomField {

  def apply(template:Field):Field =
    apply(template,UniformDistribution(1f,0f),new Random())
  def apply(template:Field, dist:DistributionType):Field =
    apply(template,dist,new Random())
  def apply(template:Field, dist:DistributionType, seed:Long):Field =
    apply(template,dist,new Random(seed))
  def apply(template:Field, dist:DistributionType, rng:Random):Field = {
    template match {
      case sf:ScalarField => scalar(sf, dist, rng)
      case vf:VectorField => vector(vf, dist, rng)
      case mf:MatrixField => matrix(mf, dist, rng)
      case _ => throw new RuntimeException("Field type not supported")
    }
  }

  private def scalar(template:ScalarField, dist:DistributionType, rng:Random):ScalarField = {
    val fieldShape = template.fieldShape

    def nextScalar(resetFirst: Boolean) =
      distFunc(rng.nextFloatResetFirstIf(resetFirst), dist)

    fieldShape.dimensions match {
      case 0 => ScalarField(nextScalar(true))
      case 1 => ScalarField(fieldShape(0),
        (i)=>nextScalar(i==0))
      case 2 => ScalarField(fieldShape(0), fieldShape(1),
        (i,j)=>nextScalar(i==0 && j==0))
      case 3 => ScalarField(fieldShape(0), fieldShape(1), fieldShape(2),
        (i,j,k)=>nextScalar(i==0 && j==0 && k==0))
      case _ => throw new RuntimeException("template has invalid number of dimensions")
    }
  }

  private def vector(template:VectorField, dist:DistributionType, rng:Random):VectorField = {
    val fieldShape = template.fieldShape
    val vectorElements = template.tensorShape(0)

    def nextScalar(resetFirst: Boolean) =
      distFunc(rng.nextFloatResetFirstIf(resetFirst), dist)

    def nextVector(firstFieldPoint: Boolean) =
      Vector(vectorElements, (i)=>nextScalar(firstFieldPoint && i==0))

    fieldShape.dimensions match {
      case 0 => VectorField(nextVector(true))
      case 1 => VectorField(fieldShape(0),
        (i)=>nextVector(i==0))
      case 2 => VectorField(fieldShape(0), fieldShape(1),
        (i,j)=>nextVector(i==0 && j==0))
      case 3 => VectorField(fieldShape(0), fieldShape(1), fieldShape(2),
        (i,j,k)=>nextVector(i==0 && j==0 && k==0))
      case _ => throw new RuntimeException("template has invalid number of dimensions")
    }
  }

  private def matrix(template:MatrixField, dist:DistributionType, rng:Random):MatrixField = {
    val fieldShape = template.fieldShape
    val matrixRows = template.tensorShape(0)
    val matrixCols = template.tensorShape(1)

    def nextScalar(resetFirst: Boolean) =
      distFunc(rng.nextFloatResetFirstIf(resetFirst), dist)

    def nextMatrix(firstFieldPoint: Boolean) =
      Matrix(matrixRows, matrixCols,
        (i,j)=>nextScalar(firstFieldPoint && i==0 && j==0))

    fieldShape.dimensions match {
      case 0 => MatrixField(nextMatrix(true))
      case 1 => MatrixField(fieldShape(0),
        (i)=>nextMatrix(i==0))
      case 2 => MatrixField(fieldShape(0), fieldShape(1),
        (i,j)=>nextMatrix(i==0 && j==0))
      case 3 =>
        MatrixField(fieldShape(0), fieldShape(1), fieldShape(2),
          (i,j,k)=>nextMatrix(i==0 && j==0 && k==0))
      case _ => throw new RuntimeException("template has invalid number of dimensions")
    }
  }
}
