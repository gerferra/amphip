package amphip.model

import scala.language.implicitConversions

import scala.collection.immutable.NumericRange

import cats.syntax.option._

import spire.math._
import spire.implicits._

import amphip.model.ast._
import amphip.model.ops._

object instances extends AllInstances

trait AllInstances extends NumInstances
  with SymInstances
  with TupleInstances
  with SetInstances
  with LogInstances
  with RefInstances
  with EqInstances
  with NEqInstances
  with LTInstances
  with LTEInstances
  with GTInstances
  with GTEInstances
  with DimenInstances
  with WithinInstances
  with AssignInstances
  with DefaultInstances
  with InInstances
  with IntegerInstances
  with BinaryInstances
  with SymbolicInstances
  with CondInstances
  with Cond1Instances
  with AddInstances
  with SubInstances
  with LessInstances
  with SumInstances
  with ProdInstances
  with MaxInstances
  with MinInstances
  with MultInstances
  with DivInstances
  with DivExactInstances
  with ModInstances
  with UnaryPlusInstances
  with UnaryMinusInstances
  with RaiseInstances
  with PipeInstances
  with DiffInstances
  with SymDiffInstances
  with InterInstances
  with SetOfInstances
  with ToInstances
  with ByInstances
  with DisjInstances
  with ForallInstances
  with ExistsInstances
  with ConjInstances
  with NegInstances
  ////
  with SizeInstances

trait NumInstances extends NumInstancesLowPriority {

  implicit def NumericAsNumLit[A: Numeric](a: A): NumExpr = {
    NumLit(a.toBigDecimal)
  }

}
trait NumInstancesLowPriority {

  implicit def NumExprAsSymExpr[A](x: A)(implicit conv: A => NumExpr): SymExpr = SymNumExpr(x)

}

trait SymInstances {

  implicit def StringAsStringLit(a: String): SymExpr = StringLit(a)
  implicit def CharAsStringLit(a: Char): SymExpr = StringLit(a.toString)

}

trait TupleInstances {
  implicit def Tuple2AsListSimpleExpr[A, B](a: A, b: B)(
    implicit conv1: A => SimpleExpr,
             conv2: B => SimpleExpr): List[SimpleExpr] = List(a, b)

  implicit def Tuple2AsListSimpleExprTupled[A, B](t: (A, B))(
    implicit conv1: A => SimpleExpr,
             conv2: B => SimpleExpr): List[SimpleExpr] = List(t._1, t._2)

  implicit def Tuple3AsListSimpleExpr[A, B, C](a: A, b: B, c: C)(
    implicit conv1: A => SimpleExpr,
             conv2: B => SimpleExpr,
             conv3: C => SimpleExpr): List[SimpleExpr] = List(a, b, c)

  implicit def Tuple4AsListSimpleExpr[A, B, C, D](a: A, b: B, c: C, d: D)(
    implicit conv1: A => SimpleExpr,
             conv2: B => SimpleExpr,
             conv3: C => SimpleExpr,
             conv4: D => SimpleExpr): List[SimpleExpr] = List(a, b, c, d)

  implicit def Tuple5AsListSimpleExpr[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E)(
    implicit conv1: A => SimpleExpr,
             conv2: B => SimpleExpr,
             conv3: C => SimpleExpr,
             conv4: D => SimpleExpr,
             conv5: E => SimpleExpr): List[SimpleExpr] = List(a, b, c, d, e)
}

trait SetInstances extends SetInstancesLowPriority { self: NumInstances =>

  implicit def SetExprAsIndEntry[A](a: A)(implicit conv: A => SetExpr): IndEntry = IndEntry(Nil, a)
  implicit def RangeAsArithSet(t: Range): ArithSet = ArithSet(t.start, t.end, if (t.step != 1) Some(t.step) else None)
  implicit def NumericRangeAsArithSet[T: Numeric](t: NumericRange[T]): ArithSet = ArithSet(t.start, t.end, Some(t.step))
  implicit def IndExprAsSetexpr[A](a: A)(implicit conv: A => IndExpr): SetExpr = IndExprSet(a)

  implicit def ListListSimpleExprAsSetLit[A](values: List[List[A]])(implicit conv: A => SimpleExpr): SetExpr = SetLit(values.map(_.map(conv)): _*)
  implicit def ListSimpleExprAsSetLit    [A](values: List[A])(implicit conv: A => SimpleExpr      ): SetExpr = SetLit(values.map(conv).map(List(_)): _*)
}

trait SetInstancesLowPriority {
  implicit def ListListSimpleExprAsSetLitLP[A](values: List[A])(implicit conv: A => List[SimpleExpr]): SetExpr = SetLit(values.map(conv): _*)
}

trait LogInstances {

  implicit def IndEntryAsIn(x: IndEntry): In = In(x.indices.map(DummyIndRef(_)), x.set)
  // XXX add a LogicExpr literal to convert any Scala.Boolean into

}

trait RefInstances {

  implicit def SetStatRefOp[B](implicit conv: B => SimpleExpr): RefOp[SetStat, B, SetRef] = new RefOp[SetStat, B, SetRef] {
    def apply(a: => SetStat, expr: List[B]): SetRef = SetRef(a, expr.map(conv))
  }

  implicit def ParamStatRefOp[B](implicit conv: B => SimpleExpr): RefOp[ParamStat, B, ParamRef] = new RefOp[ParamStat, B, ParamRef] {
    def apply(a: => ParamStat, expr: List[B]): ParamRef = ParamRef(a, expr.map(conv))
  }

  implicit def VarStatRefOp[B](implicit conv: B => SimpleExpr): RefOp[VarStat, B, VarRef] = new RefOp[VarStat, B, VarRef] {
    def apply(a: => VarStat, expr: List[B]): VarRef = VarRef(a, expr.map(conv))
  }

  implicit def ConstraintStatRefOp[B](implicit conv: B => SimpleExpr): RefOp[ConstraintStat, B, ConstraintRef] = new RefOp[ConstraintStat, B, ConstraintRef] {
    def apply(a: => ConstraintStat, expr: List[B]): ConstraintRef = ConstraintRef(a, expr.map(conv))
  }

  implicit def ObjectiveStatRefOp[B](implicit conv: B => SimpleExpr): RefOp[ObjectiveStat, B, ObjectiveRef] = new RefOp[ObjectiveStat, B, ObjectiveRef] {
    def apply(a: => ObjectiveStat, expr: List[B]): ObjectiveRef = ObjectiveRef(a, expr.map(conv))
  }
 
  def AAsRef[A, C](lhe: => A)(implicit RefOp: RefOp[A, SimpleExpr, C]): C = RefOp.apply(lhe, Nil)
  implicit def SetStatAsRef(lhe: SetStat): SetRef = AAsRef(lhe)
  implicit def ParamStatAsRef(lhe: ParamStat): ParamRef = AAsRef(lhe)
  implicit def VarStatAsRef(lhe: VarStat): VarRef = AAsRef(lhe)
  implicit def ConstraintStatAsRef(lhe: ConstraintStat): ConstraintRef = AAsRef(lhe)
  implicit def ObjectiveStatAsRef(lhe: ObjectiveStat): ObjectiveRef = AAsRef(lhe)

  implicit def DummyIndDeclAsDummyIndRef(x: DummyIndDecl): DummyIndRef = DummyIndRef(x)

  // lazy versions to allow recursive definitions
  implicit def SetStatAsRef(lhe: => SetStat): SetRef = AAsRef(lhe)
  implicit def ParamStatAsRef(lhe: => ParamStat): ParamRef = AAsRef(lhe)
  implicit def VarStatAsRef(lhe: => VarStat): VarRef = AAsRef(lhe)
  implicit def ConstraintStatAsRef(lhe: => ConstraintStat): ConstraintRef = AAsRef(lhe)
  implicit def ObjectiveStatAsRef(lhe: => ObjectiveStat): ObjectiveRef = AAsRef(lhe)

  
}

trait EqInstances extends EqInstancesLowPriority1 {

  implicit def ParamStatEqOp[A](implicit conv: A => SimpleExpr): EqOp[ParamStat, A, ParamStat] = new EqOp[ParamStat, A, ParamStat] {
    def eq(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamEq(rhe))
  }
  implicit def VarStatEqOp[A](implicit conv: A => NumExpr): EqOp[VarStat, A, VarStat] = new EqOp[VarStat, A, VarStat] {
    def eq(lhe: VarStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ VarEq(rhe))
  }

}
trait EqInstancesLowPriority1 extends EqInstancesLowPriority2 {

  implicit def SimpleExprEqOp[A, B](implicit convA: A => SimpleExpr, convB: B => SimpleExpr): EqOp[A, B, LogicExpr] = new EqOp[A, B, LogicExpr] {
    def eq(lhe: A, rhe: B) =
      Eq(left = lhe, right = rhe)
  }

}
trait EqInstancesLowPriority2 {

  implicit def LinExprEqOp[A, B](implicit convA: A => LinExpr, convB: B => LinExpr): EqOp[A, B, ConstraintStat] = new EqOp[A, B, ConstraintStat] {
    def eq(lhe: A, rhe: B) =
      EqConstraintStat(
        name = gen.ctr.freshName,
        left = lhe,
        right = rhe)
  }

}

trait NEqInstances extends NEqInstancesLowPriority {

  implicit def ParamStatNEqOp[A](implicit conv: A => SimpleExpr): NEqOp[ParamStat, A, ParamStat] = new NEqOp[ParamStat, A, ParamStat] {
    def neq(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamNEq(rhe))
  }

}
trait NEqInstancesLowPriority {

  implicit def NumExprNEqOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): NEqOp[A, B, LogicExpr] = new NEqOp[A, B, LogicExpr] {
    def neq(lhe: A, rhe: B) =
      NEq(left = lhe, right = rhe)
  }

}

trait LTInstances extends LTInstancesLowPriority {

  implicit def ParamStatLTOp[A](implicit conv: A => SimpleExpr): LTOp[ParamStat, A, ParamStat] = new LTOp[ParamStat, A, ParamStat] {
    def lt(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamLT(rhe))
  }

}
trait LTInstancesLowPriority {

  implicit def NumExprLTOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): LTOp[A, B, LogicExpr] = new LTOp[A, B, LogicExpr] {
    def lt(lhe: A, rhe: B) =
      LT(lhe, rhe)
  }

}

trait LTEInstances extends LTEInstancesLowPriority1 {

  implicit def ParamStatLTEOp[A](implicit conv: A => SimpleExpr): LTEOp[ParamStat, A, ParamStat] = new LTEOp[ParamStat, A, ParamStat] {
    def lte(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamLTE(rhe))
  }
  implicit def VarStatLTEOp[A](implicit conv: A => NumExpr): LTEOp[VarStat, A, VarStat] = new LTEOp[VarStat, A, VarStat] {
    def lte(lhe: VarStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ VarLTE(rhe))
  }

}
trait LTEInstancesLowPriority1 extends LTEInstancesLowPriority2 {

  implicit def NumExprLTEOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): LTEOp[A, B, LogicExpr] = new LTEOp[A, B, LogicExpr] {
    def lte(lhe: A, rhe: B) =
      LTE(lhe, rhe)
  }

}
trait LTEInstancesLowPriority2 {

  implicit def LinExprLTEOp[A, B](implicit convA: A => LinExpr, convB: B => LinExpr): LTEOp[A, B, ConstraintStat] = new LTEOp[A, B, ConstraintStat] {
    def lte(lhe: A, rhe: B) =
      LTEConstraintStat(
        name = gen.ctr.freshName,
        left = lhe,
        right = rhe)
  }

}

trait GTInstances extends GTInstancesLowPriority {

  implicit def ParamStatGTOp[A](implicit conv: A => SimpleExpr): GTOp[ParamStat, A, ParamStat] = new GTOp[ParamStat, A, ParamStat] {
    def gt(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamGT(rhe))
  }

}
trait GTInstancesLowPriority {

  implicit def NumExprGTOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): GTOp[A, B, LogicExpr] = new GTOp[A, B, LogicExpr] {
    def gt(lhe: A, rhe: B) =
      GT(lhe, rhe)
  }

}

trait GTEInstances extends GTEInstancesLowPriority1 {

  implicit def ParamStatGTEOp[A](implicit conv: A => SimpleExpr): GTEOp[ParamStat, A, ParamStat] = new GTEOp[ParamStat, A, ParamStat] {
    def gte(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamGTE(rhe))
  }
  implicit def VarStatGTEOp[A](implicit conv: A => NumExpr): GTEOp[VarStat, A, VarStat] = new GTEOp[VarStat, A, VarStat] {
    def gte(lhe: VarStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ VarGTE(rhe))
  }

}
trait GTEInstancesLowPriority1 extends GTEInstancesLowPriority2 {

  implicit def NumExprGTEOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): GTEOp[A, B, LogicExpr] = new GTEOp[A, B, LogicExpr] {
    def gte(lhe: A, rhe: B) =
      GTE(lhe, rhe)
  }

}
trait GTEInstancesLowPriority2 {

  implicit def LinExprGTEOp[A, B](implicit convA: A => LinExpr, convB: B => LinExpr): GTEOp[A, B, ConstraintStat] = new GTEOp[A, B, ConstraintStat] {
    def gte(lhe: A, rhe: B) =
      GTEConstraintStat(
        name = gen.ctr.freshName,
        left = lhe,
        right = rhe)
  }

}

trait DimenInstances {

  implicit val SetStatDimenOp: DimenOp[SetStat, SetStat] = new DimenOp[SetStat, SetStat] {
    def dimen(a: SetStat, n: Int) =
      a.copy(atts = a.atts :+ SetDimen(n))
  }

}

trait WithinInstances extends WithinInstancesLowPriority {

  implicit def SetStatWithinOp[A](implicit conv: A => SetExpr): WithinOp[SetStat, A, SetStat] = new WithinOp[SetStat, A, SetStat] {
    def within(lhe: SetStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ SetWithin(rhe))
  }

}
trait WithinInstancesLowPriority {

  implicit def SetExprWithinOp[A, B](implicit convA: A => SetExpr, convB: B => SetExpr): WithinOp[A, B, LogicExpr] = new WithinOp[A, B, LogicExpr] {
    def within(lhe: A, rhe: B) =
      Within(lhe, rhe)
  }

}

trait AssignInstances {

  implicit def SetStatAssignOp[A](implicit conv: A => SetExpr): AssignOp[SetStat, A, SetStat] = new AssignOp[SetStat, A, SetStat] {
    def assign(lhe: SetStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ SetAssign(rhe))
  }
  implicit def ParamStatAssignOp[A](implicit conv: A => SimpleExpr): AssignOp[ParamStat, A, ParamStat] = new AssignOp[ParamStat, A, ParamStat] {
    def assign(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamAssign(rhe))
  }

}

trait DefaultInstances {

  implicit def SetStatDefaultOp[A](implicit conv: A => SetExpr): DefaultOp[SetStat, A, SetStat] = new DefaultOp[SetStat, A, SetStat] {
    def default(lhe: SetStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ SetDefault(rhe))
  }
  implicit def ParamStatDefaultOp[A](implicit conv: A => SimpleExpr): DefaultOp[ParamStat, A, ParamStat] = new DefaultOp[ParamStat, A, ParamStat] {
    def default(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamDefault(rhe))
  }

}

trait InInstances extends InInstancesLowPriority1 { self: RefInstances =>

  implicit def ParamStatInOp[A](implicit conv: A => SetExpr): InOp[ParamStat, A, ParamStat] = new InOp[ParamStat, A, ParamStat] {
    def in(lhe: ParamStat, rhe: A) =
      lhe.copy(atts = lhe.atts :+ ParamIn(rhe))
  }

}
trait InInstancesLowPriority1 extends InInstancesLowPriority2 {

  implicit def DummyIndInOp[A, B](implicit convA: A => DummyIndDecl, convB: B => SetExpr): InOp[A, B, IndEntry] = new InOp[A, B, IndEntry] {
    def in(lhe: A, rhe: B) =
      IndEntry(List(lhe), rhe)
  }

  /* This two definitions can be replaced with GenericDummyIndInOp? */
  implicit def ListDummyIndInOp[A, B](implicit convA: A => List[DummyIndDecl], convB: B => SetExpr): InOp[A, B, IndEntry] = new InOp[A, B, IndEntry] {
    def in(lhe: A, rhe: B) =
      IndEntry(lhe, rhe)
  }

  implicit def Tuple2DummyIndIndOp[A, B](implicit convA: A => Tuple2[DummyIndDecl, DummyIndDecl], convB: B => SetExpr): InOp[A, B, IndEntry] = new InOp[A, B, IndEntry] {
    def in(lhe: A, rhe: B) =
      IndEntry(List(lhe._1, lhe._2), rhe)
  }
  
  /*
  // TODO Evaluate if it is worth to have this to replace previous two definitions
  import scala.language.higherKinds
  import shapeless.Generic

  implicit def GenericDummyIndInOp[A, F[_] <: Product, B]
    (implicit 
      convA: A => F[DummyIndDecl], 
      convB: B => SetExpr,
      gen: Generic[F[DummyIndDecl]]
    ): InOp[A, B, IndEntry] = new InOp[A, B, IndEntry] {
    def in(lhe: A, rhe: B) = {
      val x = convA(lhe)
      println(gen)
      IndEntry(x.productIterator.toList.asInstanceOf[List[DummyIndDecl]], rhe)
    }
  }
  */

}
trait InInstancesLowPriority2 {

  implicit def SimpleExprInOp[A, B](implicit convA: A => SimpleExpr, convB: B => SetExpr): InOp[A, B, LogicExpr] = new InOp[A, B, LogicExpr] {
    def in(lhe: A, rhe: B) =
      In(List(lhe), rhe)
  }

  /* This two definitions can be replaced with GenericSimpleExprInOp? */
  implicit def ListSimpleExprInOp[A, B](implicit convA: A => List[SimpleExpr], convB: B => SetExpr): InOp[A, B, LogicExpr] = new InOp[A, B, LogicExpr] {
    def in(lhe: A, rhe: B) =
      In(lhe, rhe)
  }

  implicit def Tuple2SimpleExprIndOp[A, B](implicit convA: A => Tuple2[SimpleExpr, SimpleExpr], convB: B => SetExpr): InOp[A, B, LogicExpr] = new InOp[A, B, LogicExpr] {
    def in(lhe: A, rhe: B) =
      In(List(lhe._1, lhe._2), rhe)
  }

  /*
  // TODO Evaluate if it is worth to have this to replace previous two definitions
  import scala.language.higherKinds
  import shapeless.Generic

  implicit def GenericSimpleExprInOp[A, F[_] <: Product, B]
    (implicit 
      convA: A => F[SimpleExpr], // OJO esto acepta `(X, Y, SimpleExpr)`, al cosiderar una tupla como F[_] solo importa el ultimo parámetro de tipo
      convB: B => SetExpr,
      gen: Generic[F[SimpleExpr]]
    ): InOp[A, B, LogicExpr] = new InOp[A, B, LogicExpr] {
    def in(lhe: A, rhe: B) = {
      val x = convA(lhe)
      println(gen)
      In(x.productIterator.toList.asInstanceOf[List[SimpleExpr]], rhe)
    }
  }
  */
}

trait IntegerInstances {

  implicit val ParamStatIntegerOp: IntegerOp[ParamStat] = new IntegerOp[ParamStat] {
    def integer(a: ParamStat) =
      a.copy(atts = a.atts :+ Integer)
  }
  implicit val VarStatIntegerOp: IntegerOp[VarStat] = new IntegerOp[VarStat] {
    def integer(a: VarStat) =
      a.copy(atts = a.atts :+ Integer)
  }

}

trait BinaryInstances {

  implicit val ParamStatBinaryOp: BinaryOp[ParamStat] = new BinaryOp[ParamStat] {
    def binary(a: ParamStat) =
      a.copy(atts = a.atts :+ Binary)
  }
  implicit val VarStatBinaryOp: BinaryOp[VarStat] = new BinaryOp[VarStat] {
    def binary(a: VarStat) =
      a.copy(atts = a.atts :+ Binary)
  }

}

trait SymbolicInstances {

  implicit val ParamStatSymbolicOp: SymbolicOp[ParamStat] = new SymbolicOp[ParamStat] {
    def symbolic(a: ParamStat) =
      a.copy(atts = a.atts :+ Symbolic)
  }

}

trait CondInstances extends CondInstancesLowPriority1 {

  implicit def NumExprCondOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): CondOp[LogicExpr, A, B, NumExpr] = new CondOp[LogicExpr, A, B, NumExpr] {
    def cond(test: LogicExpr)(ifTrue: A)(otherwise: B): NumExpr =
      CondNumExpr(test, ifTrue, (otherwise: NumExpr).some)
  }
  implicit def SetExprCondOp[A, B](implicit convA: A => SetExpr, convB: B => SetExpr): CondOp[LogicExpr, A, B, SetExpr] = new CondOp[LogicExpr, A, B, SetExpr] {
    def cond(test: LogicExpr)(ifTrue: A)(otherwise: B): SetExpr =
      CondSetExpr(test, ifTrue, otherwise)
  }

}
trait CondInstancesLowPriority1 extends CondInstancesLowPriority2 {

  implicit def SymExprCondOp[A, B](implicit convA: A => SymExpr, convB: B => SymExpr): CondOp[LogicExpr, A, B, SymExpr] = new CondOp[LogicExpr, A, B, SymExpr] {
    def cond(test: LogicExpr)(ifTrue: A)(otherwise: B): SymExpr =
      CondSymExpr(test, ifTrue, (otherwise: SymExpr).some)
  }

}
trait CondInstancesLowPriority2 {

  implicit def LinExprCondOp[A, B](implicit convA: A => LinExpr, convB: B => LinExpr): CondOp[LogicExpr, A, B, LinExpr] = new CondOp[LogicExpr, A, B, LinExpr] {
    def cond(test: LogicExpr)(ifTrue: A)(otherwise: B): LinExpr =
      CondLinExpr(test, ifTrue, (otherwise: LinExpr).some)
  }

}

trait Cond1Instances extends Cond1InstancesLowPriority1 {

  implicit def NumExprCond1Op[A](implicit convA: A => NumExpr): Cond1Op[LogicExpr, A, NumExpr] = new Cond1Op[LogicExpr, A, NumExpr] {
    def cond1(test: LogicExpr)(ifTrue: A): NumExpr =
      CondNumExpr(test, ifTrue)
  }

}
trait Cond1InstancesLowPriority1 extends Cond1InstancesLowPriority2 {

  implicit def SymExprCond1Op[A](implicit convA: A => SymExpr): Cond1Op[LogicExpr, A, SymExpr] = new Cond1Op[LogicExpr, A, SymExpr] {
    def cond1(test: LogicExpr)(ifTrue: A): SymExpr =
      CondSymExpr(test, ifTrue)
  }

}
trait Cond1InstancesLowPriority2 {

  implicit def LinExprCond1Op[A](implicit convA: A => LinExpr): Cond1Op[LogicExpr, A, LinExpr] = new Cond1Op[LogicExpr, A, LinExpr] {
    def cond1(test: LogicExpr)(ifTrue: A): LinExpr =
      CondLinExpr(test, ifTrue)
  }

}

trait AddInstances extends AddInstancesLowPriority {

  implicit def NumExprAddOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): AddOp[A, B, NumExpr] = new AddOp[A, B, NumExpr] {
    def add(lhe: A, rhe: B) =
      NumAdd(lhe, rhe)
  }

}
trait AddInstancesLowPriority {

  implicit def SymExprAddOp[A, B](implicit convA: A => SymExpr, convB: B => SymExpr): AddOp[A, B, SymExpr] = new AddOp[A, B, SymExpr] {
    def add(lhe: A, rhe: B) =
      Concat(lhe, rhe)
  }

  implicit def LinExprAddOp[A, B](implicit convA: A => LinExpr, convB: B => LinExpr): AddOp[A, B, LinExpr] = new AddOp[A, B, LinExpr] {
    def add(lhe: A, rhe: B) =
      LinAdd(lhe, rhe)
  }

}

trait SubInstances extends SubInstancesLowPriority {

  implicit def NumExprSubOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): SubOp[A, B, NumExpr] = new SubOp[A, B, NumExpr] {
    def sub(lhe: A, rhe: B) =
      NumSub(lhe, rhe)
  }

}
trait SubInstancesLowPriority {

  implicit def LinExprSubOp[A, B](implicit convA: A => LinExpr, convB: B => LinExpr): SubOp[A, B, LinExpr] = new SubOp[A, B, LinExpr] {
    def sub(lhe: A, rhe: B) =
      LinSub(lhe, rhe)
  }

}

trait LessInstances {

  implicit def NumExprLessOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): LessOp[A, B, NumExpr] = new LessOp[A, B, NumExpr] {
    def less(lhe: A, rhe: B) =
      NumLess(lhe, rhe)
  }

}

trait SumInstances extends SumInstancesLowPriority {

  implicit def NumExprSumOp[A](implicit convA: A => NumExpr): SumOp[IndExpr, A, NumExpr] = new SumOp[IndExpr, A, NumExpr] {
    def sum(indexing: IndExpr, integrand: A) =
      NumSum(indexing, integrand)
  }

}
trait SumInstancesLowPriority {

  implicit def LinExprSumOp[A](implicit convA: A => LinExpr): SumOp[IndExpr, A, LinExpr] = new SumOp[IndExpr, A, LinExpr] {
    def sum(indexing: IndExpr, integrand: A) =
      LinSum(indexing, integrand)
  }

}

trait ProdInstances {

  implicit def NumExprProdOp[A](implicit convA: A => NumExpr): ProdOp[IndExpr, A, NumExpr] = new ProdOp[IndExpr, A, NumExpr] {
    def prod(indexing: IndExpr, integrand: A) =
      NumProd(indexing, integrand)
  }

}

trait MaxInstances {

  implicit def NumExprMaxOp[A](implicit convA: A => NumExpr): MaxOp[IndExpr, A, NumExpr] = new MaxOp[IndExpr, A, NumExpr] {
    def max(indexing: IndExpr, integrand: A) =
      NumMax(indexing, integrand)
  }

}

trait MinInstances {

  implicit def NumExprMinOp[A](implicit convA: A => NumExpr): MinOp[IndExpr, A, NumExpr] = new MinOp[IndExpr, A, NumExpr] {
    def min(indexing: IndExpr, integrand: A) =
      NumMin(indexing, integrand)
  }

}

trait MultInstances extends MultInstancesLowPriority {

  implicit def NumExprMultOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): MultOp[A, B, NumExpr] = new MultOp[A, B, NumExpr] {
    def mult(lhe: A, rhe: B) =
      NumMult(lhe, rhe)
  }
  implicit def SetExprMultOp[A, B](implicit convA: A => SetExpr, convB: B => SetExpr): MultOp[A, B, SetExpr] = new MultOp[A, B, SetExpr] {
    def mult(lhe: A, rhe: B) =
      Cross(lhe, rhe)
  }

}
trait MultInstancesLowPriority {

  implicit def LinExprMultOp1[A, B](implicit convA: A => NumExpr, convB: B => LinExpr): MultOp[A, B, LinExpr] = new MultOp[A, B, LinExpr] {
    def mult(lhe: A, rhe: B) =
      LinMult(lhe, rhe)
  }
  implicit def LinExprMultOp2[A, B](implicit convA: A => LinExpr, convB: B => NumExpr): MultOp[A, B, LinExpr] = new MultOp[A, B, LinExpr] {
    def mult(lhe: A, rhe: B) =
      LinMult(rhe, lhe) // multiplication is commutative
  }

}

trait DivInstances extends DivInstancesLowPriority {

  implicit def NumExprDivOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): DivOp[A, B, NumExpr] = new DivOp[A, B, NumExpr] {
    def div(lhe: A, rhe: B) =
      NumDiv(lhe, rhe)
  }

}
trait DivInstancesLowPriority {

  implicit def LinExprDivOp[A, B](implicit convA: A => LinExpr, convB: B => NumExpr): DivOp[A, B, LinExpr] = new DivOp[A, B, LinExpr] {
    def div(lhe: A, rhe: B) =
      LinDiv(lhe, rhe)
  }

}

trait DivExactInstances {

  implicit def NumExprDivExactOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): DivExactOp[A, B, NumExpr] = new DivExactOp[A, B, NumExpr] {
    def divExact(lhe: A, rhe: B) =
      NumDivExact(lhe, rhe)
  }

}

trait ModInstances {

  implicit def NumExprModOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): ModOp[A, B, NumExpr] = new ModOp[A, B, NumExpr] {
    def mod(lhe: A, rhe: B) =
      NumMod(lhe, rhe)
  }

}

trait UnaryPlusInstances extends UnaryPlusInstancesLowPriority {

  implicit def NumExprUnaryPlusOp[A](implicit conv: A => NumExpr): UnaryPlusOp[A, NumExpr] = new UnaryPlusOp[A, NumExpr] {
    def plus(a: A) =
      NumUnaryPlus(a)
  }

}
trait UnaryPlusInstancesLowPriority {

  implicit def LinExprUnaryPlusOp[A](implicit conv: A => LinExpr): UnaryPlusOp[A, LinExpr] = new UnaryPlusOp[A, LinExpr] {
    def plus(a: A) =
      LinUnaryPlus(a)
  }

}

trait UnaryMinusInstances extends UnaryMinusInstancesLowPriority {

  implicit def NumExprUnaryMinusOp[A](implicit conv: A => NumExpr): UnaryMinusOp[A, NumExpr] = new UnaryMinusOp[A, NumExpr] {
    def minus(a: A) =
      NumUnaryMinus(a)
  }

}
trait UnaryMinusInstancesLowPriority {

  implicit def LinExprUnaryMinusOp[A](implicit conv: A => LinExpr): UnaryMinusOp[A, LinExpr] = new UnaryMinusOp[A, LinExpr] {
    def minus(a: A) =
      LinUnaryMinus(a)
  }

}

trait RaiseInstances {

  implicit def NumExprRaiseOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): RaiseOp[A, B, NumExpr] = new RaiseOp[A, B, NumExpr] {
    def raise(lhe: A, rhe: B) =
      NumRaise(lhe, rhe)
  }

}

trait PipeInstances {

  implicit def SetExprPipeOp[A, B](implicit convA: A => SetExpr, convB: B => SetExpr): PipeOp[A, B, SetExpr] = new PipeOp[A, B, SetExpr] {
    def pipe(lhe: A, rhe: B) =
      Union(lhe, rhe)
  }

  implicit def IndExprPipeOp[A](implicit conv: A => LogicExpr): PipeOp[IndExpr, A, IndExpr] = new PipeOp[IndExpr, A, IndExpr] {
    def pipe(lhe: IndExpr, rhe: A) = {
      val newPredicate =
        lhe.predicate.fold(conv(rhe))(x => Conj(x, rhe))

      lhe.copy(predicate = newPredicate.some)
    }
  }

  implicit def IndEntryPipeOp[A](implicit conv: A => LogicExpr): PipeOp[IndEntry, A, IndEntry] = new PipeOp[IndEntry, A, IndEntry] {
    def pipe(lhe: IndEntry, rhe: A) = {
      val newPredicate =
        lhe.predicate.fold(conv(rhe))(x => Conj(x, rhe))

      lhe.copy(predicate = newPredicate.some)
    }
  }

  /* alternative to disallow predicates on entries at the syntax level
  implicit def IndEntryPipeOp[A](implicit conv: A => LogicExpr): PipeOp[IndEntry, A, IndExpr] = new PipeOp[IndEntry, A, IndExpr] {
    def pipe(lhe: IndEntry, rhe: A) = {
      IndExpr(List(lhe), conv(rhe).some)
    }
  }
  */

}

trait DiffInstances {

  implicit def SetExprDiffOp[A, B](implicit convA: A => SetExpr, convB: B => SetExpr): DiffOp[A, B, SetExpr] = new DiffOp[A, B, SetExpr] {
    def diff(lhe: A, rhe: B) =
      Diff(lhe, rhe)
  }

}

trait SymDiffInstances {

  implicit def SetExprSymDiffOp[A, B](implicit convA: A => SetExpr, convB: B => SetExpr): SymDiffOp[A, B, SetExpr] = new SymDiffOp[A, B, SetExpr] {
    def symDiff(lhe: A, rhe: B) =
      SymDiff(lhe, rhe)
  }

}

trait InterInstances {

  implicit def SetExprInterOp[A, B](implicit convA: A => SetExpr, convB: B => SetExpr): InterOp[A, B, SetExpr] = new InterOp[A, B, SetExpr] {
    def inter(lhe: A, rhe: B) =
      Inter(lhe, rhe)
  }

}

trait SetOfInstances {

  implicit def ListSimpleExprSetOfOp[A](implicit convA: A => SimpleExpr): SetOfOp[IndExpr, A, SetExpr] = new SetOfOp[IndExpr, A, SetExpr] {
    def setOf(indexing: IndExpr, integrand: A*) =
      SetOf(indexing, integrand.map(x => x: SimpleExpr).toList)
  }

}

trait ToInstances {

  implicit def NumExprToOp[A, B](implicit convA: A => NumExpr, convB: B => NumExpr): ToOp[A, B, ArithSet] = new ToOp[A, B, ArithSet] {
    def to(t0: A, tf: B) =
      ArithSet(t0, tf)
  }

}

trait ByInstances {

  implicit def ArithSetByOp[A](implicit conv: A => NumExpr): ByOp[ArithSet, A, SetExpr] = new ByOp[ArithSet, A, SetExpr] {
    def by(exp: ArithSet, deltaT: A) =
      exp.copy(deltaT = (deltaT: NumExpr).some)
  }

}

trait DisjInstances {

  implicit def LogicExprDisjOp[A, B](implicit convA: A => LogicExpr, convB: B => LogicExpr): DisjOp[A, B, LogicExpr] = new DisjOp[A, B, LogicExpr] {
    def disj(lhe: A, rhe: B) =
      Disj(lhe, rhe)
  }

}

trait ForallInstances {

  implicit def LogicExprForallOp[A](implicit convA: A => LogicExpr): ForallOp[IndExpr, A, LogicExpr] = new ForallOp[IndExpr, A, LogicExpr] {
    def forall(indexing: IndExpr, integrand: A) =
      Forall(indexing, integrand)
  }

}

trait ExistsInstances {

  implicit def LogicExprExistsOp[A](implicit convA: A => LogicExpr): ExistsOp[IndExpr, A, LogicExpr] = new ExistsOp[IndExpr, A, LogicExpr] {
    def exists(indexing: IndExpr, integrand: A) =
      Exists(indexing, integrand)
  }

}

trait ConjInstances {

  implicit def LogicExprConjOp[A, B](implicit convA: A => LogicExpr, convB: B => LogicExpr): ConjOp[A, B, LogicExpr] = new ConjOp[A, B, LogicExpr] {
    def conj(lhe: A, rhe: B) =
      Conj(lhe, rhe)
  }

}

trait NegInstances {

  implicit def LogicExprNegOp[A](implicit conv: A => LogicExpr): NegOp[A, LogicExpr] = new NegOp[A, LogicExpr] {
    def not(a: A) =
      Neg(a)
  }

}

//// FUNCTIONS

trait SizeInstances {

  implicit def SetExprSizeOp[A](implicit conv: A => SetExpr): SizeOp[A, NumExpr] = new SizeOp[A, NumExpr] {
    def size(a: A) = Card(a)
  }
  implicit def SymExprSizeOp[A](implicit conv: A => SymExpr): SizeOp[A, NumExpr] = new SizeOp[A, NumExpr] {
    def size(a: A) = Length(a)
  }

}