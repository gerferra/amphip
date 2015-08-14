package amphip.data.dsl

import scala.annotation.implicitNotFound

import amphip.data._

object ops {
  
  @implicitNotFound("Data is not defined for ${A}, ${B}")
  trait DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData
  }
   
}