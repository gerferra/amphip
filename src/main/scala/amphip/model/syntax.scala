package amphip.model

import scala.language.implicitConversions

import scalaz.std.option._, optionSyntax._
import scalaz.std.list.listSyntax._

import spire.math._

import amphip.model.ast._
import amphip.model.ops._
import amphip.model.instances._
import amphip.collect

object syntax extends AllSyntax

trait AllSyntax {

  // TODO check the scope of the dummy indices used on the declarations
  // and maybe the arity of the references using `dimen' and `Try(eval)'
  def set(name: SymName, indexing: IndExpr): SetStat = SetStat(name, domain = indexing.some)
  def set(name: SymName, entries: IndEntry*): SetStat = SetStat(name, domain = entries.toList.toNel.map(x => IndExpr(x.list.toList)))
  def set(indexing: IndExpr)(implicit name: sourcecode.Name): SetStat = set(gen.nosuffix(name.value).freshName, indexing)
  def set(entries: IndEntry*)(implicit name: sourcecode.Name): SetStat = set(gen.nosuffix(name.value).freshName, entries: _*)
  def set(implicit name: sourcecode.Name): SetStat = set(gen.nosuffix(name.value).freshName)

  def param(name: SymName, indexing: IndExpr): ParamStat = ParamStat(name, domain = indexing.some)
  def param(name: SymName, entries: IndEntry*): ParamStat = ParamStat(name, domain = entries.toList.toNel.map(x => IndExpr(x.list.toList)))
  def param(indexing: IndExpr)(implicit name: sourcecode.Name): ParamStat = param(gen.nosuffix(name.value).freshName, indexing)
  def param(entries: IndEntry*)(implicit name: sourcecode.Name): ParamStat = param(gen.nosuffix(name.value).freshName, entries: _*)
  def param(implicit name: sourcecode.Name): ParamStat = param(gen.nosuffix(name.value).freshName)

  def xvar(name: SymName, indexing: IndExpr): VarStat = VarStat(name, domain = indexing.some)
  def xvar(name: SymName, entries: IndEntry*): VarStat = VarStat(name, domain = entries.toList.toNel.map(x => IndExpr(x.list.toList)))
  def xvar(indexing: IndExpr)(implicit name: sourcecode.Name): VarStat = xvar(gen.nosuffix(name.value).freshName, indexing)
  def xvar(entries: IndEntry*)(implicit name: sourcecode.Name): VarStat = xvar(gen.nosuffix(name.value).freshName, entries: _*)
  def xvar(implicit name: sourcecode.Name): VarStat = xvar(gen.nosuffix(name.value).freshName)

  def st(ctr: ConstraintStat)(implicit name: sourcecode.Name): ConstraintStat =
    st(gen.nosuffix(name.value).freshName, none, ctr)

  def st(name: SymName)(ctr: ConstraintStat): ConstraintStat =
    st(name, none, ctr)

  def st(domain: IndExpr)(ctr: ConstraintStat)(implicit name: sourcecode.Name): ConstraintStat =
    st(gen.nosuffix(name.value).freshName, domain.some, ctr)

  def st(entries: IndEntry*)(ctr: ConstraintStat)(implicit name: sourcecode.Name): ConstraintStat =
    st(gen.nosuffix(name.value).freshName, IndExpr(entries.toList).some, ctr)

  def st(name: SymName, entry: IndEntry, entries: IndEntry*)(ctr: ConstraintStat): ConstraintStat =
    st(name, IndExpr(entry :: entries.toList).some, ctr)

  def st(name: SymName, domain: IndExpr)(ctr: ConstraintStat): ConstraintStat =
    st(name, domain.some, ctr)

  private def st(name: SymName, domain: Option[IndExpr], ctr: ConstraintStat): ConstraintStat = ctr match {
    case ctr: EqConstraintStat => ctr.copy(name = name, domain = domain)
    case ctr: LTEConstraintStat => ctr.copy(name = name, domain = domain)
    case ctr: GTEConstraintStat => ctr.copy(name = name, domain = domain)
    case ctr: DLTEConstraintStat => ctr.copy(name = name, domain = domain)
    case ctr: DGTEConstraintStat => ctr.copy(name = name, domain = domain)
  }

  def model(obj: ObjectiveStat, constr: ConstraintStat*): Model = {
    val pf: PartialFunction[Any, Stat] = {case s: Stat => s}
    val refs = constr.flatMap(collect(_, pf)) ++ collect(obj, pf)
    val distinctRefs = refs.distinct.toList

    // sorting of elements
    val (setOrParam, rest) = distinctRefs.partition {
      case _: SetStat | _: ParamStat => true
      case _ => false
    }

    val (vars, ctrs) = rest.partition {
      case _: VarStat => true
      case _ => false
    }

    Model(setOrParam ::: vars ::: ctrs)
  }

  def dummy(name: String, synthetic: Boolean = false): DummyIndDecl = DummyIndDecl(name, synthetic)
  def dummy(implicit name: sourcecode.Name): DummyIndDecl = DummyIndDecl(gen.nosuffix(name.value).freshName, synthetic = false)

  def tup(x: DummyIndDecl, xs: DummyIndDecl*): List[DummyIndDecl] = (x +: xs).toList

  def tup(x: SimpleExpr, xs: SimpleExpr*): List[SimpleExpr] = x :: xs.toList

  def num[A: Numeric](a: A): NumExpr = a

  def str(a: String): SymExpr = a

  def str(a: Char): SymExpr = a

  def ind(entries: IndEntry*): IndExpr = IndExpr(entries.toList)

  def minimize(expr: LinExpr)(implicit name: sourcecode.Name): ObjectiveStat =
    Minimize(gen.nosuffix(name.value).freshName, expr = expr)

  def maximize(expr: LinExpr)(implicit name: sourcecode.Name): ObjectiveStat =
    Maximize(gen.nosuffix(name.value).freshName, expr = expr)

  def minimize(name: SymName)(expr: LinExpr): ObjectiveStat =
    Minimize(name = name, expr = expr)

  def maximize(name: SymName)(expr: LinExpr): ObjectiveStat =
    Maximize(name = name, expr = expr)

  implicit class RefSyntax[A](val lhe: A) {
    def apply[B](expr: SimpleExpr*)(implicit RefOp: RefOp[A, B]): B = RefOp.apply(lhe, expr.toList)

    def apply[B, C](expr: => List[C])(implicit conv: C => SimpleExpr, RefOp: RefOp[A, B]): B = RefOp.apply[C](lhe, expr)
  }

  implicit class EqSyntax[A](val lhe: A) {
    def ===[B, C](rhe: B)(implicit EqOp: EqOp[A, B, C]): C = EqOp.eq(lhe, rhe)
  }

  implicit class NEqSyntax[A](val lhe: A) {
    def =!=[B, C](rhe: B)(implicit NEqOp: NEqOp[A, B, C]): C = NEqOp.neq(lhe, rhe)
  }

  implicit class LTSyntax[A](val lhe: A) {
    def <[B, C](rhe: B)(implicit LTOp: LTOp[A, B, C]): C = LTOp.lt(lhe, rhe)
  }

  implicit class LTESyntax[A](val lhe: A) {
    def <=[B, C](rhe: B)(implicit LTEOp: LTEOp[A, B, C]): C = LTEOp.lte(lhe, rhe)
  }

  implicit class GTSyntax[A](val lhe: A) {
    def >[B, C](rhe: B)(implicit GTOp: GTOp[A, B, C]): C = GTOp.gt(lhe, rhe)
  }

  implicit class GTESyntax[A](val lhe: A) {
    def >=[B, C](rhe: B)(implicit GTEOp: GTEOp[A, B, C]): C = GTEOp.gte(lhe, rhe)
  }

  implicit class DimenSyntax[A](val a: A) {
    def dimen[B](n: Int)(implicit DimenOp: DimenOp[A, B]): B = DimenOp.dimen(a, n)
  }

  implicit class WithinSyntax[A](val lhe: A) {
    def within[B, C](rhe: B)(implicit WithinOp: WithinOp[A, B, C]): C = WithinOp.within(lhe, rhe)
  }

  implicit class AssignSyntax[A](val lhe: A) {
    def :=[B, C](rhe: B)(implicit AssignOp: AssignOp[A, B, C]): C = AssignOp.assign(lhe, rhe)
  }

  implicit class DefaultSyntax[A](val lhe: A) {
    def default[B, C](rhe: B)(implicit DefaultOp: DefaultOp[A, B, C]): C = DefaultOp.default(lhe, rhe)
  }

  implicit class InSyntax[A](val lhe: A) {
    def in[B, C](rhe: B)(implicit InOp: InOp[A, B, C]): C = InOp.in(lhe, rhe)
  }

  implicit class IntegerSyntax[A](val a: A) {
    def integer(implicit IntegerOp: IntegerOp[A]): A = IntegerOp.integer(a)
  }

  implicit class BinarySyntax[A](val a: A) {
    def binary(implicit BinaryOp: BinaryOp[A]): A = BinaryOp.binary(a)
  }

  implicit class SymbolicSyntax[A](val a: A) {
    def symbolic(implicit SymbolicOp: SymbolicOp[A]): A = SymbolicOp.symbolic(a)
  }

  def cond[A, B, C](test: LogicExpr)(ifTrue: A)(otherwise: B)(implicit CondOp: CondOp[A, B, C]): C = CondOp.cond(test)(ifTrue)(otherwise)

  def cond1[A, B](test: LogicExpr)(ifTrue: A)(implicit Cond1Op: Cond1Op[A, B]): B = Cond1Op.cond1(test)(ifTrue)

  implicit class AddSyntax[A](val lhe: A) {
    def +[B, C](rhe: B)(implicit AddOp: AddOp[A, B, C]): C = AddOp.add(lhe, rhe)
  }

  implicit class SubSyntax[A](val lhe: A) {
    def -[B, C](rhe: B)(implicit SubOp: SubOp[A, B, C]): C = SubOp.sub(lhe, rhe)
  }

  implicit class LessSyntax[A](val lhe: A) {
    def less[B, C](rhe: B)(implicit LessOp: LessOp[A, B, C]): C = LessOp.less(lhe, rhe)
  }

  def sum[A, B](indexing: IndExpr)(integrand: A)(implicit SumOp: SumOp[A, B]): B = SumOp.sum(indexing, integrand)

  def sum[A, B](entries: IndEntry*)(integrand: A)(implicit SumOp: SumOp[A, B]): B = sum(IndExpr(entries.toList))(integrand)

  def prod[A, B](indexing: IndExpr)(integrand: A)(implicit ProdOp: ProdOp[A, B]): B = ProdOp.prod(indexing, integrand)

  def prod[A, B](entries: IndEntry*)(integrand: A)(implicit ProdOp: ProdOp[A, B]): B = prod(IndExpr(entries.toList))(integrand)

  def max[A, B](indexing: IndExpr)(integrand: A)(implicit MaxOp: MaxOp[A, B]): B = MaxOp.max(indexing, integrand)

  def max[A, B](entries: IndEntry*)(integrand: A)(implicit MaxOp: MaxOp[A, B]): B = max(IndExpr(entries.toList))(integrand)

  def min[A, B](indexing: IndExpr)(integrand: A)(implicit MinOp: MinOp[A, B]): B = MinOp.min(indexing, integrand)

  def min[A, B](entries: IndEntry*)(integrand: A)(implicit MinOp: MinOp[A, B]): B = min(IndExpr(entries.toList))(integrand)

  implicit class MultSyntax[A](val lhe: A) {
    def *[B, C](rhe: B)(implicit MultOp: MultOp[A, B, C]): C = MultOp.mult(lhe, rhe)
  }

  implicit class DivSyntax[A](val lhe: A) {
    def /[B, C](rhe: B)(implicit DivOp: DivOp[A, B, C]): C = DivOp.div(lhe, rhe)
  }

  implicit class DivExactSyntax[A](val lhe: A) {
    def /~[B, C](rhe: B)(implicit DivExactOp: DivExactOp[A, B, C]): C = DivExactOp.divExact(lhe, rhe)
  }

  implicit class ModSyntax[A](val lhe: A) {
    def %[B, C](rhe: B)(implicit ModOp: ModOp[A, B, C]): C = ModOp.mod(lhe, rhe)
  }

  implicit class UnaryPlusSyntax[A](val a: A) {
    def unary_+[B](implicit UnaryPlusOp: UnaryPlusOp[A, B]): B = UnaryPlusOp.plus(a)
  }

  implicit class UnaryMinusSyntax[A](val a: A) {
    def unary_-[B](implicit UnaryMinusOp: UnaryMinusOp[A, B]): B = UnaryMinusOp.minus(a)
  }

  implicit class RaiseSyntax[A](val lhe: A) {
    def **[B, C](rhe: B)(implicit RaiseOp: RaiseOp[A, B, C]): C = RaiseOp.raise(lhe, rhe)
  }

  implicit class PipeSyntax[A](val lhe: A) {
    def |[B, C](rhe: B)(implicit PipeOp: PipeOp[A, B, C]): C = PipeOp.pipe(lhe, rhe)
  }

  implicit class DiffSyntax[A](val lhe: A) {
    def &~[B, C](rhe: B)(implicit DiffOp: DiffOp[A, B, C]): C = DiffOp.diff(lhe, rhe)
  }

  implicit class SymDiffSyntax[A](val lhe: A) {
    def ^[B, C](rhe: B)(implicit SymDiffOp: SymDiffOp[A, B, C]): C = SymDiffOp.symDiff(lhe, rhe)
  }

  implicit class InterSyntax[A](val lhe: A) {
    def &[B, C](rhe: B)(implicit InterOp: InterOp[A, B, C]): C = InterOp.inter(lhe, rhe)
  }

  def setOf[A, B](indexing: IndExpr)(integrand: A*)(implicit SetOfOp: SetOfOp[A, B]): B = SetOfOp.setOf(indexing, integrand: _*)

  def setOf[A, B](entries: IndEntry*)(integrand: A*)(implicit SetOfOp: SetOfOp[A, B]): B = setOf(IndExpr(entries.toList))(integrand: _*)

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

  implicit class BySyntax[A](val exp: A) {
    def by[B, C](deltaT: B)(implicit ByOp: ByOp[A, B, C]): C = ByOp.by(exp, deltaT)
  }

  implicit class DisjSyntax[A](val lhe: A) {
    def ||[B, C](rhe: B)(implicit DisjOp: DisjOp[A, B, C]): C = DisjOp.disj(lhe, rhe)
  }

  def forall[A, B](indexing: IndExpr)(integrand: A)(implicit ForallOp: ForallOp[A, B]): B = ForallOp.forall(indexing, integrand)

  def forall[A, B](entries: IndEntry*)(integrand: A)(implicit ForallOp: ForallOp[A, B]): B = forall(IndExpr(entries.toList))(integrand)

  def exists[A, B](indexing: IndExpr)(integrand: A)(implicit ExistsOp: ExistsOp[A, B]): B = ExistsOp.exists(indexing, integrand)

  def exists[A, B](entries: IndEntry*)(integrand: A)(implicit ExistsOp: ExistsOp[A, B]): B = exists(IndExpr(entries.toList))(integrand)

  implicit class ConjSyntax[A](val lhe: A) {
    def &&[B, C](rhe: B)(implicit ConjOp: ConjOp[A, B, C]): C = ConjOp.conj(lhe, rhe)
  }

  implicit class NegSyntax[A](val a: A) {
    def unary_![B](implicit NegOp: NegOp[A, B]): B = NegOp.not(a)
  }

  implicit class ModelSyntax(val model: Model) {
    def relax(xvar: VarStat): Model = {
      val relaxVar = xvar.relax
      replace(xvar, relaxVar)
    }

    def relax: Model = model.variables.foldLeft(model)((model, v) => model.relax(v))

    def replace[A: Manifest](target: A, replacement: A): Model = amphip.replace(model, target, replacement)

    def variables: List[VarStat] = model.statements.collect { case x: VarStat => x }

    def parameters: List[ParamStat] = model.statements.collect { case x: ParamStat => x }

    def sets: List[SetStat] = model.statements.collect { case x: SetStat => x }

    def objective: ObjectiveStat = model.statements.collect { case x: ObjectiveStat => x }.headOption.err(s"Objective statement not defined")

    def constraints: List[ConstraintStat] = model.statements.collect { case x: ConstraintStat => x }

    def param(name: String): ParamStat = parameters.find(_.name == name).err(s"Parameter `$name' not defined")

    def xvar(name: String): VarStat = variables.find(_.name == name).err(s"Variable `$name' not defined")

    def set(name: String): SetStat = sets.find(_.name == name).err(s"Set `$name' not defined")

    def ctr(name: String): ConstraintStat = constraints.find(_.name == name).err(s"Constraint `$name' not defined")

    def +:(stat: Stat): Model = {
      val refs = collect(stat, { case s: Stat => s })
      val newStatements = refs ++ model.statements
      Model(newStatements.distinct)
    }

    def :+(stat: Stat): Model = {
      val refs = collect(stat, { case s: Stat => s })
      val newStatements = model.statements ++ refs
      Model(newStatements.distinct)
    }

    def :++(stats: List[Stat]): Model = {
      val refs = stats.flatMap(collect(_, { case s: Stat => s }))
      val newStatements = model.statements ++ refs
      Model(newStatements.distinct)
    }

    def ++:(stats: List[Stat]): Model = {
      val refs = stats.flatMap(collect(_, { case s: Stat => s }))
      val newStatements = refs ++ model.statements
      Model(newStatements.distinct)
    }
  }

  implicit class IndExprSyntax(val indexing: IndExpr) {
    def sets: List[SetExpr] = indexing.entries.map(_.set)
  }

  implicit class ParamStatSyntax(val param: ParamStat) {
    def sets: List[SetExpr] = param.domain.map(_.sets) | Nil

    def isComputable: Boolean = param.atts.exists(_ match {
      case x: ParamAssign => true
      case _ => false
    })
  }

  implicit class VarStatSyntax(val xvar: VarStat) {
    def relax: VarStat = {
      val newAtts = xvar.atts.filter {
        case Binary => false
        case Integer => false
        case _ => true
      }
      xvar.copy(atts = newAtts)
    }

    def sets: List[SetExpr] = xvar.domain.map(_.sets) | Nil
  }

  //// FUNCTIONS

  // TODO add syntax for all the functions

  implicit class SizeSyntax[A](val a: A) {
    def size[B](implicit SizeOp: SizeOp[A, B]): B = SizeOp.size(a)
  }

}
