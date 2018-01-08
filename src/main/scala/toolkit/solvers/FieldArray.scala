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

import scala.reflect.ClassTag
import libcog._

/**
  * A rich class which contains a 1D scala array of fields and defines some
  * common operators on arrays of fields.
  * @author Matthew Pickett
  */
class FieldArray(val fields: Array[Field])
{
  /////////////////////
  //Simple functions
  /////////////////////
  def apply(index:Int) = this.fields(index)
  def map[B:ClassTag](f: Field =>B) = fields.map(f).toArray
  val length = fields.length
  val fieldTypes = this.map(_.fieldType)

  /////////////////////
  //Binary operators
  /////////////////////
  private def binaryMap(that: FieldArray, f:(Field, Field)=>Field):FieldArray ={
    require(this.length == 1 || that.length == 1 || this.length == that.length,
            "incompatible FieldArray lengths for binary operator")
    val thisIsSingle = this.length == 1
    val thatIsSingle = that.length ==1
    (thisIsSingle, thatIsSingle) match {
      case (true, true) => FieldArray(f(this(0), that(0)))
      case (true, false) => FieldArray(that.map((x)=>f(this(0),x)):_*)
      case (false, true) => FieldArray(this.map((x)=>f(x,that(0))):_*)
      case (false, false) => FieldArray.tabulate(length){(i)=>f(this(i), that(i))}
    }
  }

  def +(that: FieldArray) = binaryMap(that, (a,b) => a + b)
  def -(that: FieldArray) = binaryMap(that, (a,b) => a - b)
  def *(that: FieldArray) = binaryMap(that, (a,b) => a * b)
  def /(that: FieldArray) = binaryMap(that, (a,b) => a * b.reciprocal)

  def min(that: FieldArray) = binaryMap(that, (a,b) => a min b)
  def max(that: FieldArray) = binaryMap(that, (a,b) => a max b)
  def >(that: FieldArray) = binaryMap(that, (a,b) => a > b)
  def <(that: FieldArray) = binaryMap(that, (a,b) => a > b)

  /////////////////////
  //Unary operators
  /////////////////////
  private def unaryMap(f: Field => Field) = {
    new FieldArray(this.fields.map( x=>f(x) ))
  }

  def sq = unaryMap(_.sq)
  def fieldReduceSum = unaryMap(_.fieldReduceSum)
  def reduceSum = unaryMap(_.reduceSum)
  def arrayReduceSum:Field = if(length==1) fields(0) else fields.reduce(_ + _)
  def fieldReduceMax = unaryMap(_.fieldReduceMax)
  def reduceMax = unaryMap(_.reduceMax)
  def arrayReduceMax:Field = if(length==1) fields(0) else fields.reduce(_ max _)
  def signum = unaryMap(_.signum)
  def sqrt = unaryMap(_.sqrt)
  def unary_- = unaryMap(-_)
  def abs = unaryMap(_.abs)
  def flip = unaryMap(_.flip)

  def vectorElement(i:Int) = {require(onlyVectorFields()); unaryMap(_.vectorElement(i))}



  /////////////////////
  //Auxiliary functions
  /////////////////////

  /** Mutator operator. Has side effect of setting this array's feedback*/
  def <== (f: FieldArray) {
    require(this typesEqual f, "incompatible FieldArrays")
    for(i <- 0 until length){
      this(i) <== f(i)
    }
  }
  def <== (f: Field) {
    for(i <-0 until length){
      this(i) <== f
    }
  }
  def onlyVectorFields():Boolean =
    fields.map(_.tensorShape.dimensions == 1 ).reduce(_ && _)

  /** Stack an array of `ScalarField`s of the same `Shape` into a `VectorField` */
  def toVectorField:VectorField = {
    //Check to make sure all fields contained in the array are ScalarFields
    //and that they are all the same Shape
    val shapes = this.fields.map( (x)=>
      x match{
        case f:ScalarField => f.fieldShape
        case _ => throw new RuntimeException("All fields must be ScalarFields")
      })
    require(shapes.map(_ equals shapes(0)).reduce(_ && _),
            "All fields must be the same Shape")

    vectorField(this.fields.map(_.asInstanceOf[ScalarField]))
  }

  /** Comapre the types of two `FieldArray`s */
  def typesEqual(that:FieldArray) = (this.fieldTypes zip that.fieldTypes).
                                      map(x => x._1 equals x._2).
                                      reduce(_ && _)

  def probe(probeName:String = ""){
    for(i <- 0 until length){
      fields(i).probe(probeName+"_"+i)
    }
  }

  def print() {
    this.fieldTypes.foreach(println(_))
  }
}

object FieldArray {
  /**Construct with a var args of Fields*/
  def apply(fields: Field*) = {
    new FieldArray(fields.toArray)
  }

  /**Construct with a var args of FieldTypes*/
  def apply[X:ClassTag](fieldTypes: FieldType*) = {
    new FieldArray(fieldTypes.map(Field(_)).toArray)
  }

  /*Create a new empty FieldArray with the same shape as fA*/
  def copyShape(fA: FieldArray) = {
    FieldArray(fA.map(x=>Field(x.fieldType)): _*)
  }

  /** Scala collections style tabulate constructor*/
  def tabulate(N:Int)(initializer: Int=>Field) = {
    val fieldVector = Array.tabulate[Field](N){
      (i) => initializer(i)
    }
    new FieldArray(fieldVector)
  }



  implicit def fieldToFieldArray(f:Field): FieldArray = FieldArray(f)
  implicit def floatToFieldArray(f:Float): FieldArray = FieldArray(ScalarField(f))
  implicit def arrayOfFieldsToFieldArray(v:Array[Field]): FieldArray =
    FieldArray(v: _*)

  implicit def field2FieldFunctionToFieldArray2Field[F1<%Field, F2 <%Field](f:F1=>F2):FieldArray=>F2=
    (a:FieldArray) => {
      require(a.length == 1, "implicitly converted Field=>Field to " +
              "FieldArray=>Field functions can only operate on arguments" +
              " of length 1")
      f(a(0).asInstanceOf[F1])
    }

  implicit def field2FieldArrayFunctionToFieldArray2FieldArray[F1<%Field]
                                  (f:F1=>FieldArray):FieldArray=>FieldArray=
    (a:FieldArray) => {
      require(a.length == 1, "implicitly converted Field=>FieldArray to " +
              "FieldArray=>FieldFieldArray functions can only operate on arguments" +
              " of length 1")
      f(a(0).asInstanceOf[F1])
    }

  implicit def fieldArray2FieldFunctionToFieldArray2FieldArray[F1<%Field]
  (f:FieldArray=>F1):FieldArray=>FieldArray=
    (a:FieldArray) => FieldArray(f(a))

  implicit def field2FieldFunctionToFieldArray2FieldArray[F1<%Field, F2 <%Field](f:F1=>F2):FieldArray=>FieldArray=
    (a:FieldArray) => {
      require(a.length == 1, "implicitly converted Field=>Field to " +
              "FieldArray=>FieldArray functions can only operate on arguments" +
              " of length 1")
      FieldArray(f(a(0).asInstanceOf[F1]))
    }

}
