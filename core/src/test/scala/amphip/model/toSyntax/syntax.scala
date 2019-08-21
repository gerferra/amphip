package amphip.model.toSyntax

import scala.language.implicitConversions

import spire.math._

import amphip.model.ast._
import amphip.model.toSyntax.ops._
import amphip.model.toSyntax.instances._

object syntax extends AllSyntax

trait AllSyntax {

  def num[A: Numeric](a: A): NumExpr = a

  /**
   * Provides syntax for the "to" reserved word for arithmetic sets.
   * 
   * `<: AnyRef' is a hack to not compete with `Predef.xxxWrapper(Xxx):RichXxx'
   * which also provides a `to(Xxx)' method for literals in an implicit way ...
   *
   * The hack works by requiring the second argument to not be a literal (actually any `AnyVal'),
   * so, in such cases `Predef.xxxWrapper' always wins.
   *
   * The second version, `AnyRefToSyntax', is needed to permit expressions of the from:
   * `p to 3', being `p' a parameter.
   */
  implicit class ToAnyRefSyntax[A](val a: A) {
    def to[B <: AnyRef, C](tf: B)(implicit ToOp: ToOp[A, B, C]): C = ToOp.to(a, tf)
  }
  implicit class AnyRefToSyntax[A <: AnyRef](val a: A) {
    def to[B, C](tf: B)(implicit ToOp: ToOp[A, B, C]): C = ToOp.to(a, tf)
  }
  implicit def ToSyntaxInt(a: Int): ToAnyRefSyntax[Int] = ToAnyRefSyntax(a)
  implicit def ToSyntaxLong(a: Long): ToAnyRefSyntax[Long] = ToAnyRefSyntax(a)
  implicit def ToSyntaxFloat(a: Float): ToAnyRefSyntax[Float] = ToAnyRefSyntax(a)
  implicit def ToSyntaxDouble(a: Double): ToAnyRefSyntax[Double] = ToAnyRefSyntax(a)

}


