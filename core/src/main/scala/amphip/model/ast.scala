package amphip.model

// TODO add `extends Product with Serializable` where applicable.
object ast {

  final case class Model(statements: List[Stat])

  /*
   * DECLARATIONS
   */
  sealed trait Decl extends Product with Serializable {
    def name: SymName
  }

  final case class DummyIndDecl(name: SymName, synthetic: Boolean = false) extends Decl

  /*
   * STATEMENTS 
   */
  sealed trait Stat extends Decl

  final case class SetStat(
                      name: SymName,
                      alias: Option[StringLit] = None,
                      domain: Option[IndExpr] = None,
                      atts: List[SetAtt] = Nil) extends Stat

  final case class ParamStat(name: SymName,
                       alias: Option[StringLit] = None,
                       domain: Option[IndExpr] = None,
                       atts: List[ParamAtt] = Nil) extends Stat

  final case class VarStat(name: SymName,
                     alias: Option[StringLit] = None,
                     domain: Option[IndExpr] = None,
                     atts: List[VarAtt] = Nil) extends Stat

  sealed trait ConstraintStat extends Stat

  final case class EqConstraintStat(
                               name: SymName,
                               alias: Option[StringLit] = None,
                               domain: Option[IndExpr] = None,
                               left: LinExpr,
                               right: LinExpr) extends ConstraintStat

  final case class LTEConstraintStat(
                                name: SymName,
                                alias: Option[StringLit] = None,
                                domain: Option[IndExpr] = None,
                                left: LinExpr,
                                right: LinExpr) extends ConstraintStat

  final case class GTEConstraintStat(
                                name: SymName,
                                alias: Option[StringLit] = None,
                                domain: Option[IndExpr] = None,
                                left: LinExpr,
                                right: LinExpr) extends ConstraintStat

  final case class DLTEConstraintStat(
                                 name: SymName,
                                 alias: Option[StringLit] = None,
                                 domain: Option[IndExpr] = None,
                                 lower: NumExpr,
                                 expr: LinExpr,
                                 upper: NumExpr) extends ConstraintStat

  final case class DGTEConstraintStat(
                                 name: SymName,
                                 alias: Option[StringLit] = None,
                                 domain: Option[IndExpr] = None,
                                 upper: NumExpr,
                                 expr: LinExpr,
                                 lower: NumExpr) extends ConstraintStat

  sealed trait ObjectiveStat extends Stat

  final case class Minimize(
                       name: SymName,
                       alias: Option[StringLit] = None,
                       domain: Option[IndExpr] = None,
                       expr: LinExpr) extends ObjectiveStat

  final case class Maximize(
                       name: SymName,
                       alias: Option[StringLit] = None,
                       domain: Option[IndExpr] = None,
                       expr: LinExpr) extends ObjectiveStat

  sealed trait SetAtt

  final case class SetDimen(expr: Int) extends SetAtt

  final case class SetWithin(expr: SetExpr) extends SetAtt

  final case class SetAssign(expr: SetExpr) extends SetAtt

  final case class SetDefault(expr: SetExpr) extends SetAtt

  sealed trait ParamAtt

  final case class ParamLT(expr: SimpleExpr) extends ParamAtt

  final case class ParamLTE(expr: SimpleExpr) extends ParamAtt

  final case class ParamEq(expr: SimpleExpr) extends ParamAtt

  final case class ParamNEq(expr: SimpleExpr) extends ParamAtt

  final case class ParamGT(expr: SimpleExpr) extends ParamAtt

  final case class ParamGTE(expr: SimpleExpr) extends ParamAtt

  final case class ParamIn(expr: SetExpr) extends ParamAtt

  final case class ParamAssign(expr: SimpleExpr) extends ParamAtt

  final case class ParamDefault(expr: SimpleExpr) extends ParamAtt

  sealed trait VarAtt

  final case class VarGTE(expr: NumExpr) extends VarAtt

  final case class VarLTE(expr: NumExpr) extends VarAtt

  final case class VarEq(expr: NumExpr) extends VarAtt

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
   * Groups SetRef, ParamRef, VarRef, ConstraintRef, ObjectiveRef
   */
  sealed trait StatRef

  /*
   * SIMPLE
   */
  sealed trait NumExpr extends SimpleExpr with LogicExpr with LinExpr

  sealed trait SymExpr extends SimpleExpr

  /*
   * ParamStat reference with lazy semantic to allow recursive definitions.
   * Overrides `hashCode`, `equals` and `Product` methods to only consider the 
   * name of the ParamStat and avoid infinite loops.
   */
  final class ParamRef(paramThunk: () => ParamStat, val subscript: List[SimpleExpr] = Nil) extends NumExpr with SymExpr with StatRef with Product with Serializable {
    lazy val param: ParamStat = paramThunk()

    def copy(paramThunk: () => ParamStat = paramThunk, subscript: List[SimpleExpr] = subscript) = 
      new ParamRef(paramThunk, subscript)

    lazy private val product = (param.name, subscript)

    override def equals(obj: Any): Boolean = obj match {
      case that: ParamRef => (that canEqual this) && this.product.equals(that.product)
      case _ => false 
    }
    override def hashCode(): Int = this.product.hashCode()

    override def toString(): String = productPrefix + product

    // from Product
    override def canEqual(that: Any): Boolean = that.isInstanceOf[ParamRef]
    override def productArity: Int = product.productArity
    override def productElement(n: Int): Any = product.productElement(n)
    override def productPrefix: String = "ParamRef"
  }
  object ParamRef {
    def apply(param: => ParamStat, subscript: List[SimpleExpr] = Nil) = new ParamRef(() => param, subscript)

    def unapply(x: ParamRef): Option[(ParamStat, List[SimpleExpr])] =
      Some((x.param, x.subscript))
  } 

  final case class DummyIndRef(dummyInd: DummyIndDecl) extends NumExpr with SymExpr

  /*
   * NUMERIC
   */
  final case class CondNumExpr(test: LogicExpr, ifTrue: NumExpr, otherwise: Option[NumExpr] = None) extends NumExpr

  final case class NumAdd(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumSub(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumLess(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumSum(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  final case class NumProd(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  final case class NumMax(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  final case class NumMin(indexing: IndExpr, integrand: NumExpr) extends NumExpr

  final case class NumMult(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumDiv(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumDivExact(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumMod(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumUnaryPlus(x: NumExpr) extends NumExpr

  final case class NumUnaryMinus(x: NumExpr) extends NumExpr

  final case class NumRaise(left: NumExpr, right: NumExpr) extends NumExpr

  final case class NumLit(num: BigDecimal) extends NumExpr

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

  final case class Abs(x: NumExpr) extends NumFuncRef

  final case class Atan(x: NumExpr) extends NumFuncRef

  final case class Atan2(y: NumExpr, x: NumExpr) extends NumFuncRef

  final case class Card(x: SetExpr) extends NumFuncRef

  final case class Ceil(x: NumExpr) extends NumFuncRef

  final case class Cos(x: NumExpr) extends NumFuncRef

  final case class Exp(x: NumExpr) extends NumFuncRef

  final case class Floor(x: NumExpr) extends NumFuncRef

  final case class Gmtime() extends NumFuncRef

  final case class Length(x: SymExpr) extends NumFuncRef

  final case class Log(x: NumExpr) extends NumFuncRef

  final case class Log10(x: NumExpr) extends NumFuncRef

  final case class Max(x: NumExpr*) extends NumFuncRef

  final case class Min(x: NumExpr*) extends NumFuncRef

  final case class Round(x: NumExpr, n: Option[NumExpr] = None) extends NumFuncRef

  final case class Sin(x: NumExpr) extends NumFuncRef

  final case class Sqrt(x: NumExpr) extends NumFuncRef

  final case class Str2time(s: SymExpr, f: SymExpr) extends NumFuncRef

  final case class Trunc(x: NumExpr, n: Option[NumExpr] = None) extends NumFuncRef

  final case class Irand224() extends NumFuncRef

  final case class Uniform01() extends NumFuncRef

  /*
   * SYMBOLIC
   */
  final case class CondSymExpr(test: LogicExpr, ifTrue: SymExpr, otherwise: Option[SymExpr] = None) extends SymExpr

  final case class Concat(left: SymExpr, right: SymExpr) extends SymExpr

  final case class StringLit(str: String) extends SymExpr

  final case class SymNumExpr(numExpr: NumExpr) extends SymExpr

  /*
   * XXX Same comments as NumFuncRef
   */
  sealed trait SymFuncRef extends SymExpr

  final case class Substr(x: SymExpr, from: NumExpr, length: Option[NumExpr] = None) extends SymFuncRef

  final case class Time2str(t: NumExpr, f: SymExpr) extends SymFuncRef

  /*
   * SET
   */
  final case class CondSetExpr(test: LogicExpr, ifTrue: SetExpr, otherwise: SetExpr) extends SetExpr

  final case class Union(left: SetExpr, right: SetExpr) extends SetExpr

  final case class Diff(left: SetExpr, right: SetExpr) extends SetExpr

  final case class SymDiff(left: SetExpr, right: SetExpr) extends SetExpr

  final case class Inter(left: SetExpr, right: SetExpr) extends SetExpr

  final case class Cross(left: SetExpr, right: SetExpr) extends SetExpr

  final case class SetOf(indexing: IndExpr, integrand: List[SimpleExpr]) extends SetExpr

  final case class ArithSet(t0: NumExpr, tf: NumExpr, deltaT: Option[NumExpr] = None) extends SetExpr

  /*
   * SetStat reference with lazy semantic to allow recursive definitions.
   * Overrides `hashCode`, `equals` and `Product` methods to only consider the 
   * name of the SetStat and avoid infinite loops.
   */
  final class SetRef(setThunk: () => SetStat, val subscript: List[SimpleExpr] = Nil) extends SetExpr with StatRef with Product with Serializable {
    lazy val set: SetStat = setThunk()

    def copy(setThunk: () => SetStat = setThunk, subscript: List[SimpleExpr] = subscript) = 
      new SetRef(setThunk, subscript)

    lazy private val product = (set.name, subscript)

    override def equals(obj: Any): Boolean = obj match {
      case that: SetRef => (that canEqual this) && this.product.equals(that.product)
      case _ => false 
    }
    override def hashCode(): Int = this.product.hashCode()

    override def toString(): String = productPrefix + product

    // from Product
    override def canEqual(that: Any): Boolean = that.isInstanceOf[SetRef]
    override def productArity: Int = product.productArity
    override def productElement(n: Int): Any = product.productElement(n)
    override def productPrefix: String = "SetRef"
  }
  object SetRef {
    def apply(set: => SetStat, subscript: List[SimpleExpr] = Nil) = new SetRef(() => set, subscript)

    def unapply(x: SetRef): Option[(SetStat, List[SimpleExpr])] =
      Some((x.set, x.subscript))
  } 

  final case class SetLit(values: Tuple*) extends SetExpr

  /*
   * INDEXING
   */
  final case class IndExpr(entries: List[IndEntry], predicate: Option[LogicExpr] = None)

  final case class IndExprSet(indexing: IndExpr) extends SetExpr

  final case class IndEntry(indices: List[DummyIndDecl], set: SetExpr, predicate: Option[LogicExpr] = None)

  object IndEntry {
    def extract(predicate: LogicExpr): Map[DummyIndDecl, SimpleExpr] = predicate match {
      case Conj(expr1: LogicExpr, expr2: LogicExpr) => extract(expr1) ++ extract(expr2)
      case Eq(DummyIndRef(ind), expr: SimpleExpr) => Map(ind -> expr)
      case _ => Map()
    }
  }

  /*
   * LOGIC
   */
  final case class Disj(left: LogicExpr, right: LogicExpr) extends LogicExpr

  final case class Forall(indexing: IndExpr, integrand: LogicExpr) extends LogicExpr

  final case class Exists(indexing: IndExpr, integrand: LogicExpr) extends LogicExpr

  final case class Conj(left: LogicExpr, right: LogicExpr) extends LogicExpr

  final case class Neg(x: LogicExpr) extends LogicExpr

  final case class LT(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  final case class LTE(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  final case class GT(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  final case class GTE(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  final case class Eq(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  final case class NEq(left: SimpleExpr, right: SimpleExpr) extends LogicExpr

  final case class In(values: List[SimpleExpr], set: SetExpr) extends LogicExpr

  final case class NotIn(values: List[SimpleExpr], set: SetExpr) extends LogicExpr

  final case class Within(left: SetExpr, right: SetExpr) extends LogicExpr

  final case class NotWithin(left: SetExpr, right: SetExpr) extends LogicExpr

  /*
   * LINEAR
   */
  final case class CondLinExpr(test: LogicExpr, ifTrue: LinExpr, otherwise: Option[LinExpr] = None) extends LinExpr

  final case class LinAdd(left: LinExpr, right: LinExpr) extends LinExpr

  final case class LinSub(left: LinExpr, right: LinExpr) extends LinExpr

  final case class LinSum(indexing: IndExpr, integrand: LinExpr) extends LinExpr
  // XXX artificial construct to handle more easily this case when interpreting the model
  final case class LinSumExp(summands: List[LinExpr]) extends LinExpr

  final case class LinMult(left: NumExpr, right: LinExpr) extends LinExpr

  final case class LinDiv(left: LinExpr, right: NumExpr) extends LinExpr

  final case class LinUnaryPlus(x: LinExpr) extends LinExpr

  final case class LinUnaryMinus(x: LinExpr) extends LinExpr

  /*
   * VarStat reference with lazy semantic to allow recursive definitions.
   * Overrides `hashCode`, `equals` and `Product` methods to only consider the 
   * name of the VarStat and avoid infinite loops.
   */
  final class VarRef(varThunk: () => VarStat, val subscript: List[SimpleExpr] = Nil) extends LinExpr with StatRef with Product with Serializable {
    lazy val xvar: VarStat = varThunk()

    def copy(varThunk: () => VarStat = varThunk, subscript: List[SimpleExpr] = subscript) = 
      new VarRef(varThunk, subscript)

    lazy private val product = (xvar.name, subscript)

    override def equals(obj: Any): Boolean = obj match {
      case that: VarRef => (that canEqual this) && this.product.equals(that.product)
      case _ => false 
    }
    override def hashCode(): Int = this.product.hashCode()

    override def toString(): String = productPrefix + product

    // from Product
    override def canEqual(that: Any): Boolean = that.isInstanceOf[VarRef]
    override def productArity: Int = product.productArity
    override def productElement(n: Int): Any = product.productElement(n)
    override def productPrefix: String = "VarRef"
  }
  object VarRef {
    def apply(xvar: => VarStat, subscript: List[SimpleExpr] = Nil) = new VarRef(() => xvar, subscript)

    def unapply(x: VarRef): Option[(VarStat, List[SimpleExpr])] =
      Some((x.xvar, x.subscript))
  } 

  /*
   * STAT
   */
  sealed trait RowRef extends StatRef // to group ConstraintRef and ObjectiveRef ...
  final case class ConstraintRef(crt: ConstraintStat, subscript: List[SimpleExpr] = Nil) extends RowRef
  final case class ObjectiveRef(obj: ObjectiveStat, subscript: List[SimpleExpr] = Nil) extends RowRef
  


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
        import scala.collection.JavaConverters._
        import scalaz._, Scalaz._
        val hints = hinted.asScala.valuesIterator.map(x => s"- $x").toList.toNel.cata(_.toList.mkString("\nhints:\n", "\n", ""), "")
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
