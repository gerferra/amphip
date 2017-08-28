package amphip

import scala.annotation.implicitNotFound

import scalaz.std.option.optionSyntax._
import scalaz.std.list.listSyntax._
import scalaz.syntax.foldable1._

import spire.math

import amphip.model.ast._
import amphip.model.dsl._

object dimen {

  @implicitNotFound("Dimen is not defined for ${A}")
  trait Dimen[A] {
    def dimen(a: A): Int
  }

  def apply[A](x: A)(implicit Dimen: Dimen[A]): Int = Dimen.dimen(x)

  def from[A](f: A => Int): Dimen[A] = new Dimen[A] {
    def dimen(a: A): Int = f(a)
  }

  implicit val SetStatDimen: Dimen[SetStat] = from({
    case x: SetStat =>
      val dimensions = x.atts.map {
        case SetDimen(n) => n
        case SetWithin(expr) => dimen(expr)
        case SetAssign(expr) => dimen(expr)
        case SetDefault(expr) => dimen(expr)
      }

      // all the dimensions must be the same. check?
      dimensions.headOption | 1
  })

  implicit val SetExprDimen: Dimen[SetExpr] = from({
    case CondSetExpr(_, ifTrue, _) =>
      // both branches must have the same dimension. check?
      dimen(ifTrue)

    case Union(left, right) => math.max(dimen(left), dimen(right))

    case Diff(left, _) => dimen(left)

    case SymDiff(left, right) => math.max(dimen(left), dimen(right))

    case Inter(left, _) => dimen(left)

    case Cross(left, right) => dimen(left) + dimen(right)

    case SetOf(_, integrand) => integrand.size

    case ArithSet(_, _, _) => 1

    case SetRef(set, _) => dimen(set)

    case SetLit(values @ _*) =>
      values.headOption.map {
        case Nil => 1 // empty set has dimen 1 in MathProg
        case _ :: Nil => 1
        case l => l.size
      } | 1

    case IndExprSet(indexing) => dimen(indexing)
  })

  implicit val IndExprDimen: Dimen[IndExpr] = from({
    case x: IndExpr =>
      val entryDimen = x.entries.map(x => dimen(x.set))
      entryDimen.toNel.cata(_.foldLeft1((n1, n2) => n1 + n2), 1)
  })

}