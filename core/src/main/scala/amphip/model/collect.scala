package amphip.model

import scala.annotation.implicitNotFound

import amphip.model.ast._

object collect {

  @implicitNotFound("Collect is not defined for ${A}")
  trait Collect[A] {
    def collect[B](in: A, matching: PartialFunction[Any, B]): List[B]
  }

  def apply[A, B](in: A, matching: PartialFunction[Any, B])(implicit Collect: Collect[A]): List[B] = Collect.collect(in, matching)

  private[this] val collect_ = amphip.model.collect // to disambiguate with Collect#collect method

  // MODEL

  implicit val ModelCollect: Collect[Model] = new Collect[Model] {
    def collect[B](x: Model, pf: PartialFunction[Any, B]): List[B] = x match {

      case x @ Model(statements) => statements.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

    }
  }

  // STATEMENTS
  implicit val StatCollect: Collect[Stat] = new Collect[Stat] {
    def collect[B](x: Stat, pf: PartialFunction[Any, B]): List[B] = x match {

      case x: SetStat => collect_(x, pf)

      case x: ParamStat => collect_(x, pf)

      case x: VarStat => collect_(x, pf)

      case x: ConstraintStat => collect_(x, pf)

      case x: ObjectiveStat => collect_(x, pf)
    }
  }

  implicit val SetStatCollect: Collect[SetStat] = new Collect[SetStat] {
    def collect[B](x: SetStat, pf: PartialFunction[Any, B]): List[B] = x match {

      case SetStat(_, _, domain, atts) =>
        domain.toList.flatMap(collect_(_, pf)) ++ atts.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

    }
  }

  implicit val ParamStatCollect: Collect[ParamStat] = new Collect[ParamStat] {
    def collect[B](x: ParamStat, pf: PartialFunction[Any, B]): List[B] = x match {

      case ParamStat(_, _, domain, atts) =>
        domain.toList.flatMap(collect_(_, pf)) ++ atts.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

    }
  }

  implicit val VarStatCollect: Collect[VarStat] = new Collect[VarStat] {
    def collect[B](x: VarStat, pf: PartialFunction[Any, B]): List[B] = x match {

      case VarStat(_, _, domain, atts) =>
        domain.toList.flatMap(collect_(_, pf)) ++ atts.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

    }
  }

  implicit val ConstraintStatCollect: Collect[ConstraintStat] = new Collect[ConstraintStat] {
    def collect[B](x: ConstraintStat, pf: PartialFunction[Any, B]): List[B] = x match {

      case EqConstraintStat(_, _, domain, left, right) =>
        domain.toList.flatMap(collect_(_, pf)) ++
          collect_(left, pf) ++
          collect_(right, pf) ++
          pf.lift(x).toList

      case LTEConstraintStat(_, _, domain, left, right) =>
        domain.toList.flatMap(collect_(_, pf)) ++
          collect_(left, pf) ++
          collect_(right, pf) ++
          pf.lift(x).toList

      case GTEConstraintStat(_, _, domain, left, right) =>
        domain.toList.flatMap(collect_(_, pf)) ++
          collect_(left, pf) ++
          collect_(right, pf) ++
          pf.lift(x).toList

      case DGTEConstraintStat(_, _, domain, upper, expr, lower) =>
        domain.toList.flatMap(collect_(_, pf)) ++
          collect_(upper, pf) ++
          collect_(expr, pf) ++
          collect_(lower, pf) ++
          pf.lift(x).toList

      case DLTEConstraintStat(_, _, domain, lower, expr, upper) =>
        domain.toList.flatMap(collect_(_, pf)) ++
          collect_(lower, pf) ++
          collect_(expr, pf) ++
          collect_(upper, pf) ++
          pf.lift(x).toList

    }
  }

  implicit val ObjectiveStatCollect: Collect[ObjectiveStat] = new Collect[ObjectiveStat] {
    def collect[B](x: ObjectiveStat, pf: PartialFunction[Any, B]): List[B] = x match {

      case Minimize(_, _, domain, expr) =>
        domain.toList.flatMap(collect_(_, pf)) ++
          collect_(expr, pf) ++
          pf.lift(x).toList

      case Maximize(_, _, domain, expr) =>
        domain.toList.flatMap(collect_(_, pf)) ++
          collect_(expr, pf) ++
          pf.lift(x).toList

    }
  }

  implicit val SetAttCollect: Collect[SetAtt] = new Collect[SetAtt] {
    def collect[B](x: SetAtt, pf: PartialFunction[Any, B]): List[B] = x match {

      case SetDimen(n) => pf.lift(n).toList ++ pf.lift(x).toList

      case SetWithin(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case SetAssign(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case SetDefault(expr) => collect_(expr, pf) ++ pf.lift(x).toList

    }
  }

  implicit val ParamAttCollect: Collect[ParamAtt] = new Collect[ParamAtt] {
    def collect[B](x: ParamAtt, pf: PartialFunction[Any, B]): List[B] = x match {

      case ParamLT(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamLTE(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamEq(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamNEq(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamGT(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamGTE(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamIn(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamAssign(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case ParamDefault(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Integer => pf.lift(x).toList

      case Binary => pf.lift(x).toList

      case Symbolic => pf.lift(x).toList

    }
  }

  implicit val VarAttCollect: Collect[VarAtt] = new Collect[VarAtt] {
    def collect[B](x: VarAtt, pf: PartialFunction[Any, B]): List[B] = x match {

      case VarLTE(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case VarEq(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case VarGTE(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Integer => pf.lift(x).toList

      case Binary => pf.lift(x).toList

    }
  }

  // EXPRESSIONS
  implicit val ExprCollect: Collect[Expr] = new Collect[Expr] {
    def collect[B](x: Expr, pf: PartialFunction[Any, B]): List[B] = x match {

      case x: SimpleExpr => collect_(x, pf)

      case x: SetExpr => collect_(x, pf)

      case x: LogicExpr => collect_(x, pf)

      case x: LinExpr => collect_(x, pf)

    }
  }

  // SIMPLE
  implicit val SimpleExprCollect: Collect[SimpleExpr] = new Collect[SimpleExpr] {
    def collect[B](x: SimpleExpr, pf: PartialFunction[Any, B]): List[B] = x match {

      case x: NumExpr => collect_(x, pf)

      case x: SymExpr => collect_(x, pf)

    }
  }

  implicit val ParamRefCollect: Collect[ParamRef] = new Collect[ParamRef] {
    def collect[B](x: ParamRef, pf: PartialFunction[Any, B]): List[B] = x match {

      case ParamRef(param, subscript) =>
        collect_(param, pf) ++ subscript.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

    }
  }

  implicit val DummyIndRefCollect: Collect[DummyIndRef] = new Collect[DummyIndRef] {
    def collect[B](x: DummyIndRef, pf: PartialFunction[Any, B]): List[B] = collect_(x.dummyInd, pf) ++ pf.lift(x).toList
  }

  // NUMERIC
  implicit val NumExprCollect: Collect[NumExpr] = new Collect[NumExpr] {
    def collect[B](x: NumExpr, pf: PartialFunction[Any, B]): List[B] = x match {

      case CondNumExpr(test, ifTrue, otherwise) =>
        collect_(test, pf) ++ collect_(ifTrue, pf) ++ otherwise.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case NumAdd(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NumSub(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NumLess(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NumSum(indexing, integrand) => collect_(indexing, pf) ++ collect_(integrand, pf) ++ pf.lift(x).toList

      case NumProd(indexing, integrand) => collect_(indexing, pf) ++ collect_(integrand, pf) ++ pf.lift(x).toList

      case NumMax(indexing, integrand) => collect_(indexing, pf) ++ collect_(integrand, pf) ++ pf.lift(x).toList

      case NumMin(indexing, integrand) => collect_(indexing, pf) ++ collect_(integrand, pf) ++ pf.lift(x).toList

      case NumMult(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NumDiv(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NumDivExact(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NumMod(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NumUnaryPlus(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case NumUnaryMinus(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case NumRaise(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case x: ParamRef => collect_(x, pf)

      case x: DummyIndRef => collect_(x, pf)

      case x: NumFuncRef => collect_(x, pf)

      case x: NumLit => pf.lift(x).toList

    }
  }

  implicit val NumFuncRefCollect: Collect[NumFuncRef] = new Collect[NumFuncRef] {
    def collect[B](x: NumFuncRef, pf: PartialFunction[Any, B]): List[B] = x match {

      case Abs(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Atan(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Atan2(x1, x2) => collect_(x1, pf) ++ collect_(x2, pf) ++ pf.lift(x).toList

      case Card(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Ceil(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Cos(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Exp(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Floor(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Gmtime() => pf.lift(x).toList

      case Length(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Log(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Log10(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Max(expr @ _*) => expr.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case Min(expr @ _*) => expr.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case Round(expr, n) => collect_(expr, pf) ++ n.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case Sin(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Sqrt(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case Str2time(s, f) => collect_(s, pf) ++ collect_(f, pf) ++ pf.lift(x).toList

      case Trunc(expr, n) => collect_(expr, pf) ++ n.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case Irand224() => pf.lift(x).toList

      case Uniform01() => pf.lift(x).toList

    }
  }

  // SYMBOLIC
  implicit val SymExprCollect: Collect[SymExpr] = new Collect[SymExpr] {
    def collect[B](x: SymExpr, pf: PartialFunction[Any, B]): List[B] = x match {

      case CondSymExpr(test, ifTrue, otherwise) =>
        collect_(test, pf) ++ collect_(ifTrue, pf) ++ otherwise.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case Concat(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case SymNumExpr(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case x: ParamRef => collect_(x, pf)

      case x: DummyIndRef => collect_(x, pf)

      case x: SymFuncRef => collect_(x, pf)

      case x: StringLit => pf.lift(x).toList

    }
  }

  implicit val SymFuncRefCollect: Collect[SymFuncRef] = new Collect[SymFuncRef] {
    def collect[B](x: SymFuncRef, pf: PartialFunction[Any, B]): List[B] = x match {

      case Substr(expr, from, length) =>
        collect_(expr, pf) ++ collect_(from, pf) ++ length.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case Time2str(t, f) => collect_(t, pf) ++ collect_(f, pf) ++ pf.lift(x).toList

    }
  }

  // SET
  implicit val SetExprCollect: Collect[SetExpr] = new Collect[SetExpr] {
    def collect[B](x: SetExpr, pf: PartialFunction[Any, B]): List[B] = x match {

      case CondSetExpr(test, ifTrue, otherwise) =>
        collect_(test, pf) ++ collect_(ifTrue, pf) ++ collect_(otherwise, pf) ++ pf.lift(x).toList

      case Union(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case Diff(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case SymDiff(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case Inter(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case Cross(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case SetOf(indexing, integrand) => collect_(indexing, pf) ++ integrand.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case ArithSet(t0, tf, deltaT) => collect_(t0, pf) ++ collect_(tf, pf) ++ deltaT.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case SetRef(set, subscript) => collect_(set, pf) ++ subscript.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case SetLit(values @ _*) => values.toList.flatMap(_.flatMap(collect_(_, pf))) ++ pf.lift(x).toList

      case IndExprSet(indexing) => collect_(indexing, pf) ++ pf.lift(x).toList

    }
  }

  //INDEXING
  implicit val IndExprCollect: Collect[IndExpr] = new Collect[IndExpr] {
    def collect[B](x: IndExpr, pf: PartialFunction[Any, B]): List[B] = x match {
      case IndExpr(entries, predicate) =>
        entries.flatMap(collect_(_, pf)) ++ predicate.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList
    }
  }

  implicit val IndEntryCollect: Collect[IndEntry] = new Collect[IndEntry] {
    def collect[B](x: IndEntry, pf: PartialFunction[Any, B]): List[B] = x match {

      case IndEntry(indices, expr, filter) =>
        indices.flatMap(collect_(_, pf)) ++ collect_(expr, pf) ++ filter.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList
    }
  }

  implicit val DummyIndDeclCollect: Collect[DummyIndDecl] = new Collect[DummyIndDecl] {
    def collect[B](x: DummyIndDecl, pf: PartialFunction[Any, B]): List[B] = x match {

      case DummyIndDecl(_, _) => pf.lift(x).toList

    }
  }

  // LOGIC
  implicit val LogicExprCollect: Collect[LogicExpr] = new Collect[LogicExpr] {
    def collect[B](x: LogicExpr, pf: PartialFunction[Any, B]): List[B] = x match {

      case Disj(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case Forall(indexing, integrand) =>
        collect_(indexing, pf) ++ collect_(integrand, pf) ++ pf.lift(x).toList

      case Exists(indexing, integrand) =>
        collect_(indexing, pf) ++ collect_(integrand, pf) ++ pf.lift(x).toList

      case Conj(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case Neg(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case LT(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case LTE(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case GT(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case GTE(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NEq(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case Eq(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case NotIn(values, set) => values.flatMap(collect_(_, pf)) ++ collect_(set, pf) ++ pf.lift(x).toList

      case In(values, set) => values.flatMap(collect_(_, pf)) ++ collect_(set, pf) ++ pf.lift(x).toList

      case NotWithin(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case Within(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case x: NumExpr => collect_(x, pf)

    }
  }

  // LINEAR
  implicit val LinExprCollect: Collect[LinExpr] = new Collect[LinExpr] {
    def collect[B](x: LinExpr, pf: PartialFunction[Any, B]): List[B] = x match {

      case CondLinExpr(test, ifTrue, otherwise) =>
        collect_(test, pf) ++ collect_(ifTrue, pf) ++ otherwise.toList.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

      case LinAdd(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case LinSub(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case LinSum(indexing, integrand) => collect_(indexing, pf) ++ collect_(integrand, pf) ++ pf.lift(x).toList
      case LinSumExp(summands) => summands.collect(pf) ++ pf.lift(x).toList

      case LinMult(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case LinDiv(left, right) => collect_(left, pf) ++ collect_(right, pf) ++ pf.lift(x).toList

      case LinUnaryPlus(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case LinUnaryMinus(expr) => collect_(expr, pf) ++ pf.lift(x).toList

      case x: VarRef => collect_(x, pf)

      case x: NumExpr => collect_(x, pf)

    }
  }

  implicit val VarRefCollect: Collect[VarRef] = new Collect[VarRef] {
    def collect[B](x: VarRef, pf: PartialFunction[Any, B]): List[B] = x match {

      case VarRef(xvar, subscript) =>
        collect_(xvar, pf) ++ subscript.flatMap(collect_(_, pf)) ++ pf.lift(x).toList

    }
  }

}