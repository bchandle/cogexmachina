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
package toolkit.filtering.wavelets

import libcog._
import scala.annotation.tailrec

/** Wavelet*/
trait Wavelet{
  val coeffs:Array[Double]

  private def generateFilterBank(elems:Int):VectorField = {
    val n = coeffs.length
    val filterLen = n + (n+1) % 2
    val h = List.tabulate[Float](filterLen){
      (i) => if (i < n) coeffs(i).toFloat else 0f
    }
    val g = List.tabulate[Float](filterLen){
      (i) => if (i < n) (math.pow(-1,i)*coeffs(n-i-1)).toFloat else 0f
    }

    //define the filter bank to be used for convolution
    VectorField(filterLen,filterLen, (r,c) => Vector(4*elems,
      i => i/elems match{
        case 0 => h(r)*h(c)
        case 1 => h(r)*g(c)
        case 2 => g(r)*h(c)
        case 3 => g(r)*g(c)
        case _ => throw new RuntimeException("")
    }))
  }

  final def transform(x:VectorField, layers:Int):List[VectorField] = {
    require(layers > 0)
    require(x.fieldShape.dimensions == 2, "Only implemented for 2D fields")
    val cast = x match {
      case vf:VectorField => vf
      case _ => throw new RuntimeException("Wavelet transformation only valid on VectorFields")
    }

    val elements = cast.tensorShape(0)
    val fb = generateFilterBank(elements)
    val l1:VectorField = projectFrame(x, fb, BorderCyclic, DownsampleOutputConvolution(2))
    val lowPassIndices = VectorField(Vector(elements,(i)=>i))
    val highPassIndices = VectorField(Vector(3*elements, (i)=>i+elements))

    transform(List(l1), fb, lowPassIndices, highPassIndices, layers-1)
  }

  @tailrec private def transform(x:List[VectorField],
                                 fb:VectorField,
                                 lpi:VectorField,
                                 hpi:VectorField,
                                 layers:Int):List[VectorField] =
    if(layers == 0) x
    else {
      //pull out the lowpass portion
      val prevLowPass:VectorField = vectorElements(x.head, lpi)
      //pull out the highpass portion
      val prevHighPass:VectorField = vectorElements(x.head, hpi)

      //perform the convolution
      val result = projectFrame(prevLowPass, fb, BorderCyclic,
        DownsampleOutputConvolution(2))

      //append the highpass and lowpass to the transformed list
      transform(result :: prevHighPass :: x.tail, fb, lpi, hpi, layers-1)
    }


  final def invert(x:List[Field]):VectorField = {
    require(x.length > 0)
    require(x.map(_.fieldShape.dimensions==2).reduce(_&&_),
      "Only implemented for 2D fields")
    require(
      if(x.length > 1)
        x.tail.map(_.tensorShape(0) % 3 == 0).reduce(_&&_)
      else
        x.head.tensorShape(0) % 3 == 0
      , "Incorrect number of vector field elements")
    require(x.head.tensorShape(0) % 4 == 0,
      "Incorrect number of field elements")

    val elements = x.head.tensorShape(0)/4
    val fb = generateFilterBank(elements)
    val r = Vector(4*elements,(i)=>elements*(i%4)+i/4)

    val reindex = VectorField(r)

    def inv(cur:VectorField) = {
      val proj = convolve(cur, fb, BorderCyclic, UpsampleInputConvolution(2))
      blockReduceSum(vectorElements(proj, reindex), 4)

    }

    //@todo: make this more efficient
    def appendVectors(a:VectorField, b:VectorField) = {
      val aType = a.fieldType
      val bType = b.fieldType
      require(aType.tensorOrder == 1)
      require(bType.tensorOrder == 1)
      require(aType.elementType == bType.elementType)
      require(aType.fieldShape == bType.fieldShape)
      val aScalars = Array.tabulate[ScalarField](a.tensorShape(0)){ i=>a.vectorElement(i) }
      val bScalars = Array.tabulate[ScalarField](b.tensorShape(0)){ i=>b.vectorElement(i) }
      val appended = bScalars ++ aScalars
      vectorField(appended)
    }
    /*
    mock up user GPU kernel. need to expose big tensor addressing to user kernels
    def appendVectors(a:Field,b:Field):Field ={
      val aType = a.fieldType
      val bType = b.fieldType
      require(aType.tensorOrder == 1)
      require(bType.tensorOrder == 1)
      require(aType.elementType == bType.elementType)
      require(aType.fieldShape == bType.fieldShape)
      val outputType = new FieldType(
        a.fieldShape,
        Shape(a.tensorShape(0)+b.tensorShape(0)),
        a.fieldType.elementType)
      GPUOperator(outputType){
        _if(_tensorElement < aType.tensorShape(0) ){
          _return(_readElement(a, _row, _column, _tensorElement))
        }
        _else{
          _return(_readElement(b, _row, _column, _tensorElement - cutoff))
        }

      }
    }*/


    x.tail.foldRight(inv(x.head))((a,b)=>inv(appendVectors(a,b)))
  }
}

/**The Haar wavelet. */
case object Haar extends Wavelet{
  val coeffs = Array(0.707107, 0.707107)
}
/**The even index Daubechies wavelets. */
case object D4 extends Wavelet{
  val coeffs = Array(0.482963, 0.836516, 0.224144, -0.12941)
}
case object D6 extends Wavelet{
  val coeffs = Array(0.332671, 0.806892, 0.459878, -0.135011, -0.0854413, 0.0352263)
}
case object D8 extends Wavelet{
  val coeffs = Array(0.230378, 0.714847, 0.630882, -0.0279838, -0.187035, 0.0308414,
    0.032883, -0.0105974)
}
case object D10 extends Wavelet{
  val coeffs = Array(0.160102, 0.603829, 0.724309, 0.138428, -0.242295, -0.0322449,
    0.0775715, -0.00624149, -0.0125808, 0.00333754)
}
case object D12 extends Wavelet{
  val coeffs = Array(0.111541, 0.494624, 0.751134, 0.31525, -0.226265, -0.129767,
    0.0975016, 0.0275229, -0.031582, 0.000553665, 0.00478004, -0.0010748)
}
case object D14 extends Wavelet{
  val coeffs = Array(0.0778521, 0.396539, 0.729132, 0.469782, -0.143906, -0.224036,
    0.0713094, 0.0806126, -0.0380299, -0.0165745, 0.012551, 0.000429921,
    -0.00180312, 0.000353553)
}
case object D16 extends Wavelet{
  val coeffs = Array(0.0544158, 0.312872, 0.675631, 0.585355, -0.0158291, -0.284016,
    0.000472347, 0.128747, -0.0173693, -0.0440883, 0.013981, 0.00874609,
    -0.00487197, -0.000391737, 0.000675287, -0.00011738)
}
case object D18 extends Wavelet{
  val coeffs = Array(0.0380779, 0.243835, 0.604823, 0.657288, 0.133197, -0.293274,
    -0.0968408, 0.148541, 0.0307257, -0.0676328, 0.000251023, 0.0223617,
    -0.00472347, -0.004278, 0.00184555, 0.000230517, -0.00025173,
    0.0000393151)
}
case object D20 extends Wavelet{
  val coeffs = Array(0.0266701, 0.188177, 0.527201, 0.688459, 0.281172, -0.249846,
    -0.195946, 0.127369, 0.0930574, -0.0713941, -0.0294575, 0.0332127,
    0.00360624, -0.0107332, 0.001393, 0.00199404, -0.000685894,
    -0.000116673, 0.0000933381, -0.0000132936)
}