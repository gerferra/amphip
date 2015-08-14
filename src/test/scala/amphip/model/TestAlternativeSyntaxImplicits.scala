package amphip.model

import amphip.model.ast._

class TestAlternativeSyntaxImplicits {

  val a = ParamStat("a")
  val b = ParamStat("b")

  val x = VarStat("x")
  val y = VarStat("y")

  val i = DummyIndDecl("i")
  val j = DummyIndDecl("j")

  /*
   * The problem in the three first cases is that we need that Scala do 
   * the implicit search using both the type of the object on which the 
   * method is being applied, and the type of the argument.
   * 
   * With implicit classes, the implicit search stops once a "valid"
   * conversion is found for the object on which the method is being applied.
   */

  import TestAlternativeSyntaxImplicits._

  new ForallSyntax {
    val test1 = i <= j // LogicExpr

    val test2 = a <= b // ParamStat

    //val test3 = (x: LinExpr) <= y // tries to use `NumExprSyntax' because is defined for all A and have higher priority. Scala don't use the type of `y' to determine the implicit class to use

    //val test4 = x <= y // tries to use VarStatSyntax because the type of x. Scala don't use the type of `y' to determine the implicit class to use
  }

  new ViewSyntax {
    val test1 = i <= j // LogicExpr

    val test2 = a <= b // ParamStat

    val test3 = (x: LinExpr) <= y // now success because LinExpr can't be converted to NumExpr, so it can't use `NumExprSyntax' 

    //val test4 = x <= y // tries to use VarStatSyntax because the type of x. Scala don't use the type of `y' to determine the implicit class to use
  }

  new DirectSyntax {
    val test1 = i <= j // LogicExpr

    val test2 = a <= b // ParamStat

    val test3 = (x: LinExpr) <= y // now success because LinExpr can't be converted to NumExpr, so it can't use `NumExprSyntax' 

    //val test4 = x <= y // tries to use VarStatSyntax because the type of x. Scala don't use the type of `y' to determine the implicit class to use
  }

  ////

  new CustomHierarchy {
    val a = ParamStat()
    val b = ParamStat()

    val x = VarStat()
    val y = VarStat()

    val i = DummyIndDecl()
    val j = DummyIndDecl()

    val test1 = i <= j // LogicExpr

    val test2 = a <= b // ParamStat

    val test3 = (x: LinExpr) <= y // ConstraintStat 

    val test4 = x <= y // ConstraintStat

    val test5 = x <= b // VarStat
  }

}

object TestAlternativeSyntaxImplicits {

  import scala.language.implicitConversions

  import spire.math._
  import spire.implicits._

  trait BaseSyntax {
    implicit def NumericAsNumLit[A: Numeric](a: A): NumExpr = {
      NumLit(a.toBigDecimal)
    }

    implicit class ParamStatSyntax(lhe: ParamStat) {
      def <=[A <% SimpleExpr](rhe: A): ParamStat = lhe.copy(atts = lhe.atts :+ ParamLTE(rhe))
    }

    implicit class VarStatSyntax(lhe: VarStat) {
      def <=[A <% NumExpr](rhe: A): VarStat = lhe.copy(atts = lhe.atts :+ VarLTE(rhe))
    }

    implicit def DummyIndDeclAsDummyIndRef(x: DummyIndDecl): DummyIndRef = DummyIndRef(x)
    implicit def SetStatRef(lhe: SetStat): SetExpr = SetRef(lhe)
    implicit def ParamStatAsRef(lhe: ParamStat): NumExpr with SymExpr = ParamRef(lhe)
    implicit def VarStatAsRef(lhe: VarStat): LinExpr = VarRef(lhe)
  }

  ////

  trait ForallSyntax extends BaseSyntax with ForallSyntaxLowPriority1

  trait ForallSyntaxLowPriority1 extends ForallSyntaxLowPriority2 {
    implicit class NumExprSyntax[A](lhe: A) {
      def <=[B](rhe: B)(implicit convA: A => NumExpr, convB: B => NumExpr): LogicExpr = LTE(lhe, rhe)
    }
  }

  trait ForallSyntaxLowPriority2 {
    implicit class LinExprSyntax[A](lhe: A) {
      def <=[B](rhe: B)(implicit convA: A => LinExpr, convB: B => LinExpr): ConstraintStat =
        LTEConstraintStat(
          name = gen.ctr.freshName,
          left = lhe,
          right = rhe)
    }
  }

  ////

  trait ViewSyntax extends BaseSyntax with ViewSyntaxLowPriority1

  trait ViewSyntaxLowPriority1 extends ViewSyntaxLowPriority2 {
    implicit class NumExprSyntax[A <% NumExpr](lhe: A) {
      def <=[B <% NumExpr](rhe: B): LogicExpr = LTE(lhe, rhe)
    }
  }

  trait ViewSyntaxLowPriority2 {
    implicit class LinExprSyntax[A <% LinExpr](lhe: A) {
      def <=[B <% LinExpr](rhe: B): ConstraintStat =
        LTEConstraintStat(
          name = gen.ctr.freshName,
          left = lhe,
          right = rhe)
    }
  }

  //// 

  trait DirectSyntax extends BaseSyntax with DirectSyntaxLowPriority1

  trait DirectSyntaxLowPriority1 extends DirectSyntaxLowPriority2 {
    implicit class NumExprSyntax[A <% NumExpr](lhe: A) {
      def <=(rhe: NumExpr): LogicExpr = LTE(lhe, rhe)
    }
  }

  trait DirectSyntaxLowPriority2 {
    implicit class LinExprSyntax[A <% LinExpr](lhe: A) {
      def <=(rhe: LinExpr): ConstraintStat =
        LTEConstraintStat(
          name = gen.ctr.freshName,
          left = lhe,
          right = rhe)
    }
  }

  ////

  trait CustomHierarchy {

    sealed trait LogicExpr
    case class LTE(left: NumExpr, right: NumExpr) extends LogicExpr
    case class Eq(left: NumExpr, right: NumExpr) extends LogicExpr

    trait LinExpr {
      def <=(rhe: LinExpr): ConstraintStat = LTEConstraintStat(this, rhe)
      def ===(rhe: LinExpr): ConstraintStat = EqConstraintStat(this, rhe)
    }

    sealed trait ConstraintStat
    case class LTEConstraintStat(left: LinExpr, right: LinExpr) extends ConstraintStat
    case class EqConstraintStat(left: LinExpr, right: LinExpr) extends ConstraintStat

    trait NumExpr extends LinExpr {
      def <=(rhe: NumExpr): LogicExpr = LTE(this, rhe)
      def ===(rhe: NumExpr): LogicExpr = Eq(this, rhe)
    }

    case class ParamRef(p: ParamStat) extends NumExpr
    case class DummyIndRef(d: DummyIndDecl) extends NumExpr
    case class VarRef(v: VarStat) extends LinExpr

    implicit def DummyIndDeclAsDummyIndRef(x: DummyIndDecl): DummyIndRef = DummyIndRef(x)
    implicit def ParamStatAsRef(lhe: ParamStat): NumExpr = ParamRef(lhe)
    implicit def VarStatAsRef(lhe: VarStat): LinExpr = VarRef(lhe)

    case class ParamStat(atts: List[ParamAtt] = Nil) {
      def <=(rhe: NumExpr): ParamStat = copy(atts = atts :+ ParamLTE(rhe))
      def <=(rhe: LinExpr): ConstraintStat = LTEConstraintStat(this, rhe)
      def ===(rhe: NumExpr): ParamStat = copy(atts = atts :+ ParamEq(rhe))
      def ===(rhe: LinExpr): ConstraintStat = EqConstraintStat(this, rhe)
    }

    case class VarStat(atts: List[VarAtt] = Nil) {
      def <=(rhe: NumExpr): VarStat = copy(atts = atts :+ VarLTE(rhe))
      def <=(rhe: LinExpr): ConstraintStat = LTEConstraintStat(this, rhe)
      def ===(rhe: NumExpr): VarStat = copy(atts = atts :+ VarEq(rhe))
      def ===(rhe: LinExpr): ConstraintStat = EqConstraintStat(this, rhe)
    }

    case class DummyIndDecl() {
      def <=(rhe: NumExpr): LogicExpr = LTE(this, rhe)
      def <=(rhe: LinExpr): ConstraintStat = LTEConstraintStat(this, rhe)
      def ===(rhe: NumExpr): LogicExpr = Eq(this, rhe)
      def ===(rhe: LinExpr): ConstraintStat = EqConstraintStat(this, rhe)
    }

    sealed trait ParamAtt
    case class ParamLTE(expr: NumExpr) extends ParamAtt
    case class ParamEq(expr: NumExpr) extends ParamAtt

    sealed trait VarAtt
    case class VarLTE(expr: NumExpr) extends VarAtt
    case class VarEq(expr: NumExpr) extends VarAtt

  }
}