package amphip.model

object ast {

  case class Model(statements: List[Stat])

  /*
   * DECLARATIONS
   */
  sealed trait Decl {
    def name: SymName
  }

  case class DummyIndDecl(name: SymName, synthetic: Boolean = false) extends Decl

  /*
   * STATEMENTS 
   */
  sealed trait Stat extends Decl

  case class SetStat(
                      name: SymName,
                      alias: Option[StringLit] = None,
                      domain: Option[IndExpr] = None,
                      atts: List[SetAtt] = Nil) extends Stat

  case class ParamStat(name: SymName,
                       alias: Option[StringLit] = None,
                       domain: Option[IndExpr] = None,
                       atts: List[ParamAtt] = Nil) extends Stat

  case class VarStat(name: SymName,
                     alias: Option[StringLit] = None,
                     domain: Option[IndExpr] = None,
                     atts: List[VarAtt] = Nil) extends Stat

  sealed trait ConstraintStat extends Stat

  case class EqConstraintStat(
                               name: SymName,
                               alias: Option[StringLit] = None,
                               domain: Option[IndExpr] = None,
                               left: LinExpr,
                               right: LinExpr) extends ConstraintStat

  case class LTEConstraintStat(
                                name: SymName,
                                alias: Option[StringLit] = None,
                                domain: Option[IndExpr] = None,
                                left: LinExpr,
                                right: LinExpr) extends ConstraintStat

  case class GTEConstraintStat(
                                name: SymName,
                                alias: Option[StringLit] = None,
                                domain: Option[IndExpr] = None,
                                left: LinExpr,
                                right: LinExpr) extends ConstraintStat

  case class DLTEConstraintStat(
                                 name: SymName,
                                 alias: Option[StringLit] = None,
                                 domain: Option[IndExpr] = None,
                                 lower: NumExpr,
                                 expr: LinExpr,
                                 upper: NumExpr) extends ConstraintStat

  case class DGTEConstraintStat(
                                 name: SymName,
                                 alias: Option[StringLit] = None,
                                 domain: Option[IndExpr] = None,
                                 upper: NumExpr,
                                 expr: LinExpr,
                                 lower: NumExpr) extends ConstraintStat

  sealed trait ObjectiveStat extends Stat

  case class Minimize(
                       name: SymName,
                       alias: Option[StringLit] = None,
                       domain: Option[IndExpr] = None,
                       expr: LinExpr) extends ObjectiveStat

  case class Maximize(
                       name: SymName,
                       alias: Option[StringLit] = None,
                       domain: Option[IndExpr] = None,
                       expr: LinExpr) extends ObjectiveStat

  sealed trait SetAtt

  case class SetDimen(expr: Int) extends SetAtt

  case class SetWithin(expr: SetExpr) extends SetAtt

  case class SetAssign(expr: SetExpr) extends SetAtt

  case class SetDefault(expr: SetExpr) extends SetAtt

  sealed trait ParamAtt

  case class ParamLT(expr: SimpleExpr) extends ParamAtt

  case class ParamLTE(expr: SimpleExpr) extends ParamAtt

  case class ParamEq(expr: SimpleExpr) extends ParamAtt

  case class ParamNEq(expr: SimpleExpr) extends ParamAtt

  case class ParamGT(expr: SimpleExpr) extends ParamAtt

  case class ParamGTE(expr: SimpleExpr) extends ParamAtt

  case class ParamIn(expr: SetExpr) extends ParamAtt

  case class ParamAssign(expr: SimpleExpr) extends ParamAtt

  case class ParamDefault(expr: SimpleExpr) extends ParamAtt

  sealed trait VarAtt

  case class VarGTE(expr: NumExpr) extends VarAtt

  case class VarLTE(expr: NumExpr) extends VarAtt

  case class VarEq(expr: NumExpr) extends VarAtt

  case object Integer extends ParamAtt with VarAtt

  case object Binary extends ParamAtt with VarAtt

  case object Symbolic extends ParamAtt

  /*
   * EXPRESSIONS
   */
  sealed trait Expr

  sealed trait SimpleExpr extends Expr // groups NumExpr and SymExpr ...

  sealed trait SetExpr extends Expr

  sealed trait LogicExpr extends Expr

  sealed trait LinExpr extends Expr

  /*
   * SIMPLE
   */
  sealed trait NumExpr extends LogicExpr with SimpleExpr with LinExpr

  sealed trait SymExpr extends SimpleExpr

  case class ParamRef(param: ParamStat, subscript: List[SimpleExpr] = Nil) extends NumExpr with SymExpr

  case class DummyIndRef(dummyInd: DummyIndDecl) extends NumExpr with SymExpr

  /*
   * NUMERIC
   */
  case class CondNumExpr(test: LogicExpr, ifTrue: NumExpr, otherwise: Option[NumExpr] = None) extends NumExpr

  case class NumAdd(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumSub(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumLess(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumSum(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  case class NumProd(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  case class NumMax(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  case class NumMin(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  case class NumMult(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumDiv(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumDivExact(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumMod(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumUnaryPlus(x: NumExpr) extends NumExpr

  case class NumUnaryMinus(x: NumExpr) extends NumExpr

  case class NumRaise(left: NumExpr, right: NumExpr) extends NumExpr

  case class NumLit(num: BigDecimal) extends NumExpr

  /*
   * XXX handle functions just by their "shapes"?
   * NumExpr => NumExpr
   * NumExpr, NumExpr => NumExpr
   * SetExpr => NumExpr
   * () => NumExpr
   * SymExpr => NumExpr
   * NumExpr* => NumExpr 
   * ....
   */
  sealed trait NumFuncRef extends NumExpr

  case class Abs(x: NumExpr) extends NumFuncRef

  case class Atan(x: NumExpr) extends NumFuncRef

  case class Atan2(y: NumExpr, x: NumExpr) extends NumFuncRef

  case class Card(x: SetExpr) extends NumFuncRef

  case class Ceil(x: NumExpr) extends NumFuncRef

  case class Cos(x: NumExpr) extends NumFuncRef

  case class Exp(x: NumExpr) extends NumFuncRef

  case class Floor(x: NumExpr) extends NumFuncRef

  case class Gmtime() extends NumFuncRef

  case class Length(x: SymExpr) extends NumFuncRef

  case class Log(x: NumExpr) extends NumFuncRef

  case class Log10(x: NumExpr) extends NumFuncRef

  case class Max(x: NumExpr*) extends NumFuncRef

  case class Min(x: NumExpr*) extends NumFuncRef

  case class Round(x: NumExpr, n: Option[NumExpr] = None) extends NumFuncRef

  case class Sin(x: NumExpr) extends NumFuncRef

  case class Sqrt(x: NumExpr) extends NumFuncRef

  case class Str2time(s: SymExpr, f: SymExpr) extends NumFuncRef

  case class Trunc(x: NumExpr, n: Option[NumExpr] = None) extends NumFuncRef

  case class Irand224() extends NumFuncRef

  case class Uniform01() extends NumFuncRef

  /*
   * SYMBOLIC
   */
  case class CondSymExpr(test: LogicExpr, ifTrue: SymExpr, otherwise: Option[SymExpr] = None) extends SymExpr

  case class Concat(left: SymExpr, right: SymExpr) extends SymExpr

  case class StringLit(str: String) extends SymExpr

  case class SymNumExpr(numExpr: NumExpr) extends SymExpr

  /*
   * XXX Same comments as NumFuncRef
   */
  sealed trait SymFuncRef extends SymExpr

  case class Substr(x: SymExpr, from: NumExpr, length: Option[NumExpr] = None) extends SymFuncRef

  case class Time2str(t: NumExpr, f: SymExpr) extends SymFuncRef

  /*
   * SET
   */
  case class CondSetExpr(test: LogicExpr, ifTrue: SetExpr, otherwise: SetExpr) extends SetExpr

  case class Union(left: SetExpr, right: SetExpr) extends SetExpr

  case class Diff(left: SetExpr, right: SetExpr) extends SetExpr

  case class SymDiff(left: SetExpr, right: SetExpr) extends SetExpr

  case class Inter(left: SetExpr, right: SetExpr) extends SetExpr

  case class Cross(left: SetExpr, right: SetExpr) extends SetExpr

  case class SetOf(indexing: IndExpr, integrand: List[SimpleExpr]) extends SetExpr

  case class ArithSet(t0: NumExpr, tf: NumExpr, deltaT: Option[NumExpr] = None) extends SetExpr

  case class SetRef(set: SetStat, subscript: List[SimpleExpr] = Nil) extends SetExpr

  case class SetLit(values: Tuple*) extends SetExpr

  /*
   * INDEXING
   */
  case class IndExpr(entries: List[IndEntry], predicate: Option[LogicExpr] = None)

  case class IndExprSet(indexing: IndExpr) extends SetExpr

  case class IndEntry(indices: List[DummyIndDecl], set: SetExpr, predicate: Option[LogicExpr] = None)

  object IndEntry {
    def extract(predicate: LogicExpr): Map[DummyIndDecl, SimpleExpr] = predicate match {
      case Conj(expr1: LogicExpr, expr2: LogicExpr) => extract(expr1) ++ extract(expr2)
      case Eq(DummyIndRef(ind), expr: SimpleExpr) => Map(ind -> expr)
      case other => Map()
    }
  }

  /*
   * LOGIC
   */
  case class Disj(left: LogicExpr, right: LogicExpr) extends LogicExpr

  case class Forall(indexing: IndExpr, integrand: LogicExpr) extends LogicExpr

  case class Exists(indexing: IndExpr, integrand: LogicExpr) extends LogicExpr

  case class Conj(left: LogicExpr, right: LogicExpr) extends LogicExpr

  case class Neg(x: LogicExpr) extends LogicExpr

  case class LT(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  case class LTE(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  case class GT(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  case class GTE(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  case class Eq(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  case class NEq(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  case class In(values: List[SimpleExpr], set: SetExpr) extends LogicExpr

  case class NotIn(values: List[SimpleExpr], set: SetExpr) extends LogicExpr

  case class Within(left: SetExpr, right: SetExpr) extends LogicExpr

  case class NotWithin(left: SetExpr, right: SetExpr) extends LogicExpr

  /*
   * LINEAR
   */
  case class CondLinExpr(test: LogicExpr, ifTrue: LinExpr, otherwise: Option[LinExpr] = None) extends LinExpr

  case class LinAdd(left: LinExpr, right: LinExpr) extends LinExpr

  case class LinSub(left: LinExpr, right: LinExpr) extends LinExpr

  case class LinSum(indexing: IndExpr, integrand: LinExpr) extends LinExpr

  case class LinMult(left: NumExpr, right: LinExpr) extends LinExpr

  case class LinDiv(left: LinExpr, right: NumExpr) extends LinExpr

  case class LinUnaryPlus(x: LinExpr) extends LinExpr

  case class LinUnaryMinus(x: LinExpr) extends LinExpr

  case class VarRef(xvar: VarStat, subscript: List[SimpleExpr] = Nil) extends LinExpr

  /*
  * BASIC
  */
  type SymName = String

  type Tuple = List[SimpleExpr]

  /*
   * UTIL 
   */

  import java.util.concurrent.atomic.AtomicInteger
  import java.util.concurrent.ConcurrentHashMap

  val gen = newGen

  def newGen: Gen = new Gen {}

  trait Gen {

    trait FreshNames { self =>
      def prefix: String
      def suffix: String = "_"

      private[this] val count = new AtomicInteger(-1)
      private[this] val hinted = new ConcurrentHashMap[String, FreshNames] // collection.mutable.Map.empty[String, FreshNames]

      private[this] def countStr = {
        val newVal = count.incrementAndGet
        if (newVal == 0) "" else newVal.toString
      }

      def freshName = {
        s"$prefix${ countStr }$suffix"
      }

      def apply(hint: String): FreshNames = apply(Some(hint))

      def apply(hint: Option[String]): FreshNames = hint match {
        case None => this
        case Some(h) =>
          hinted.putIfAbsent(h, new FreshNames {
            def prefix = h
            override def suffix = self.suffix
          })
          hinted.get(h)
      }

      override def toString = {
        import scala.collection.JavaConversions._
        import scalaz._, Scalaz._
        val hints = hinted.valuesIterator.map(x => s"- $x").toList.toNel.cata(_.toList.mkString("\nhints:\n", "\n", ""), "")
        s"freshNames for prefix=$prefix, count=${ count.get }$hints"
      }

    }

    object ctr extends FreshNames {
      def prefix = "ctr"
    }

    object obj extends FreshNames {
      def prefix = "obj"
    }

    object set extends FreshNames {
      def prefix = "X"
    }

    object param extends FreshNames {
      def prefix = "p"
    }

    object xvar extends FreshNames {
      def prefix = "x"
    }

    object dummy extends FreshNames {
      def prefix = "d"
    }

    object nosuffix extends FreshNames {
      def prefix = "n"
      override def suffix = ""
    }

  }

}