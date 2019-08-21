package amphip.data

import scala.annotation.implicitNotFound


object ops {
  
  @implicitNotFound("Data is not defined for ${A}, ${B}")
  trait DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData
  }
   
}