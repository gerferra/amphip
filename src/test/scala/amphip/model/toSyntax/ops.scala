package amphip.model.toSyntax

import scala.annotation.implicitNotFound

object ops extends Ops

trait Ops {

  @implicitNotFound("To is not defined for ${A}, ${B}")
  trait ToOp[A, B, C] {
    def to(t0: A, tf: B): C
  }

}