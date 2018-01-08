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
package toolkit.dynamicalsystems

import libcog._

/** Kalman filter for modeling linear dynamical systems
  @param model the dynamical model describing motion and measurement
  @param z the measurement vector
  @param v the uncertainty of the measurement vector
  @author Matthew Pickett
  */
class KalmanFilter(model:LinearSystemModel, z: VectorField, v:VectorField,
                   name:String="KalmanFilter")
        {
  private val A = MatrixField(model.A)
  private val H = MatrixField(model.H)

  private val stateVars = model.stateVars
  private val measVars = model.measVars
  val x = VectorField(Shape(), Shape(stateVars))

  private def identity(size:Int) = Matrix(size, size,
    (i:Int,j:Int)=> if(i==j)1f else 0f)


  private val Qmat = MatrixField(identity(stateVars))

  private val vDiag = Array.tabulate(measVars){
    (i) =>
      val curMat = new Matrix(measVars,measVars)
      curMat.update(i,i,1f)
      MatrixField(curMat)*v.vectorElement(i)
  }.reduce(_+_)

  private val Rmat = vDiag


  //matrix defs
  private val Imat = MatrixField(identity(stateVars))
  val Pmat = MatrixField(identity(stateVars))

  //predictor
  private val xPred = A transform x
  private val PmatPred = A.transform(Pmat).transform(A.transposeMatrices) + Qmat
  //Kalman gain
  private val K1 = PseudoInvert(H.transform(PmatPred).transform(H.transposeMatrices) + Rmat)
  private val Kmat = PmatPred.transform(H.transposeMatrices).transform(K1)
  //corrector
  x <==  xPred + Kmat.transform(z - H.transform(xPred))
  Pmat <== (Imat - Kmat.transform(H)) transform PmatPred
}
