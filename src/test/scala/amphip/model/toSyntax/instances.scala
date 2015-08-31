package amphip.model.toSyntax

import scala.language.implicitConversions

import scala.collection.immutable.NumericRange

import spire.math._
import spire.implicits._

import amphip.model.toSyntax.ops._
import amphip.model.ast._

object instances extends AllInstances

trait AllInstances extends NumInstances
  with ToInstances
  
trait NumInstances {

  implicit def NumericAsNumLit[A: Numeric](a: A): NumExpr = NumLit(a.toBigDecimal)

}

trait ToInstances {

  implicit def NumExprToOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): ToOp[A, B, ArithSet] = new ToOp[A, B, ArithSet] {
    def to(t0: A, tf: B) =
      ArithSet(t0, tf)
  }

}

