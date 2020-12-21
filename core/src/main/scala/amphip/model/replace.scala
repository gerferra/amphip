package amphip.model

import scala.annotation.implicitNotFound

import amphip.model.ast._

object replace {
  
  @implicitNotFound("Replace is not defined for ${A}")
  trait Replace[A] {
    def rep[B](in: A, previousValue: B, replacement: B): A
  }
  
  def apply[A, B](in: A, target: B, replacement: B)(implicit Replace: Replace[A]): A = Replace.rep(in, target, replacement)
  
  /**
  * separates the case when the thing being replaced is the `x' itself (x == pv)
  * from other cases, so Scala can do exhaustiveness check on the `otherwise' match
  */
  private def rep_[A, B](x: A, pv: B, nv: B)(otherwise: A => A): A = x match {
    case `pv` => nv.asInstanceOf[A] // x is an A /\ pv is a B /\ x == pv ===> A is B ===> nv is an A
    case _ => otherwise(x)
  }
  
  // MODEL
  
  implicit val ModelReplace: Replace[Model] = new Replace[Model] {
    def rep[B](x: Model, pv: B, nv: B): Model = rep_(x, pv, nv) {
      case Model(statements) => Model(statements.map(replace(_, pv, nv)))
    }
  }
  
  // STATEMENTS
  
  implicit val StatReplace: Replace[Stat] = new Replace[Stat] {
    def rep[B](x: Stat, pv: B, nv: B): Stat = rep_(x, pv, nv) {
      case x: SetStat => replace(x, pv, nv)
      
      case x: ParamStat => replace(x, pv, nv)
      
      case x: VarStat => replace(x, pv, nv)
      
      case x: ConstraintStat => replace(x, pv, nv)
      
      case x: ObjectiveStat => replace(x, pv, nv)
    }
  }
  
  implicit val SetStatReplace: Replace[SetStat] = new Replace[SetStat] {
    def rep[B](x: SetStat, pv: B, nv: B): SetStat = rep_(x, pv, nv) {
      case SetStat(name, alias, domain, atts) =>
        SetStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), atts.map(replace(_, pv, nv)))
    }
  }
  
  implicit val ParamStatReplace: Replace[ParamStat] = new Replace[ParamStat] {
    def rep[B](x: ParamStat, pv: B, nv: B): ParamStat = rep_(x, pv, nv) {
      case ParamStat(name, alias, domain, atts) =>
        ParamStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), atts.map(replace(_, pv, nv)))
    }
  }
  
  implicit val VarStatReplace: Replace[VarStat] = new Replace[VarStat] {
    def rep[B](x: VarStat, pv: B, nv: B): VarStat = rep_(x, pv, nv) {
      case VarStat(name, alias, domain, atts) =>
        VarStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), atts.map(replace(_, pv, nv)))
    }
  }
  
  implicit val ConstraintStatReplace: Replace[ConstraintStat] = new Replace[ConstraintStat] {
    def rep[B](x: ConstraintStat, pv: B, nv: B): ConstraintStat = rep_(x, pv, nv) {
      case EqConstraintStat(name, alias, domain, left, right) =>
        EqConstraintStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), replace(left, pv, nv), replace(right, pv, nv))
      
      case LTEConstraintStat(name, alias, domain, left, right) =>
        LTEConstraintStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), replace(left, pv, nv), replace(right, pv, nv))
      
      case GTEConstraintStat(name, alias, domain, left, right) =>
        GTEConstraintStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), replace(left, pv, nv), replace(right, pv, nv))
      
      case DGTEConstraintStat(name, alias, domain, upper, expr, lower) =>
        DGTEConstraintStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), replace(upper, pv, nv), replace(expr, pv, nv), replace(lower, pv, nv))
      
      case DLTEConstraintStat(name, alias, domain, lower, expr, upper) =>
        DLTEConstraintStat(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), replace(lower, pv, nv), replace(expr, pv, nv), replace(upper, pv, nv))
    }
  }
  
  implicit val ObjectiveStatReplace: Replace[ObjectiveStat] = new Replace[ObjectiveStat] {
    def rep[B](x: ObjectiveStat, pv: B, nv: B): ObjectiveStat = rep_(x, pv, nv) {
      case Minimize(name, alias, domain, expr) =>
        Minimize(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), replace(expr, pv, nv))
      
      case Maximize(name, alias, domain, expr) =>
        Maximize(replace(name, pv, nv), alias.map(replace(_, pv, nv)), domain.map(replace(_, pv, nv)), replace(expr, pv, nv))
    }
  }
  
  implicit val SetAttReplace: Replace[SetAtt] = new Replace[SetAtt] {
    def rep[B](x: SetAtt, pv: B, nv: B): SetAtt = rep_(x, pv, nv) {
      
      case SetDimen(n) => SetDimen(replace(n, pv, nv))
      
      case SetWithin(expr) => SetWithin(replace(expr, pv, nv))
      
      case SetAssign(expr) => SetAssign(replace(expr, pv, nv))
      
      case SetDefault(expr) => SetDefault(replace(expr, pv, nv))
      
    }
  }
  
  implicit val ParamAttReplace: Replace[ParamAtt] = new Replace[ParamAtt] {
    def rep[B](x: ParamAtt, pv: B, nv: B): ParamAtt = rep_(x, pv, nv) {
      
      case ParamLT(expr) => ParamLT(replace(expr, pv, nv))
      
      case ParamLTE(expr) => ParamLTE(replace(expr, pv, nv))
      
      case ParamEq(expr) => ParamEq(replace(expr, pv, nv))
      
      case ParamNEq(expr) => ParamNEq(replace(expr, pv, nv))
      
      case ParamGT(expr) => ParamGT(replace(expr, pv, nv))
      
      case ParamGTE(expr) => ParamGTE(replace(expr, pv, nv))
      
      case ParamIn(expr) => ParamIn(replace(expr, pv, nv))
      
      case ParamAssign(expr) => ParamAssign(replace(expr, pv, nv))
      
      case ParamDefault(expr) => ParamDefault(replace(expr, pv, nv))
      
      // covered by rep_
      case x => x
      
    }
  }
  
  implicit val VarAttReplace: Replace[VarAtt] = new Replace[VarAtt] {
    def rep[B](x: VarAtt, pv: B, nv: B): VarAtt = rep_(x, pv, nv) {
      case VarLTE(expr) => VarLTE(replace(expr, pv, nv))
      
      case VarEq(expr) => VarEq(replace(expr, pv, nv))
      
      case VarGTE(expr) => VarGTE(replace(expr, pv, nv))
      
      case x => x
    }
  }
  
  // EXPRESSIONS
  implicit val ExprReplace: Replace[Expr] = new Replace[Expr] {
    def rep[B](x: Expr, pv: B, nv: B): Expr = rep_(x, pv, nv) {
      case x: SimpleExpr => replace(x, pv, nv)
      
      case x: SetExpr => replace(x, pv, nv)
      
      case x: LogicExpr => replace(x, pv, nv)
      
      case x: LinExpr => replace(x, pv, nv)
    }
  }
  
  // SIMPLE
  implicit val SimpleExprReplace: Replace[SimpleExpr] = new Replace[SimpleExpr] {
    def rep[B](x: SimpleExpr, pv: B, nv: B): SimpleExpr = rep_(x, pv, nv) {
      case x: SymExpr => replace(x, pv, nv)
      case x: NumExpr => replace(x, pv, nv)
    }
  }
  
  implicit val ParamRefReplace: Replace[ParamRef] = new Replace[ParamRef] {
    def rep[B](x: ParamRef, pv: B, nv: B): ParamRef = rep_(x, pv, nv) {
      case ParamRef(param, subscript) => ParamRef(replace(param, pv, nv), subscript.map(replace(_, pv, nv)))
    }
  }
  
  implicit val DummyIndRefReplace: Replace[DummyIndRef] = new Replace[DummyIndRef] {
    def rep[B](x: DummyIndRef, pv: B, nv: B): DummyIndRef = rep_(x, pv, nv) {
      case DummyIndRef(dummy) => DummyIndRef(replace(dummy, pv, nv))
    }
  }
  
  // NUMERIC
  implicit val NumExprReplace: Replace[NumExpr] = new Replace[NumExpr] {
    def rep[B](x: NumExpr, pv: B, nv: B): NumExpr = rep_(x, pv, nv) {
      case CondNumExpr(test, ifTrue, otherwise) =>
        CondNumExpr(replace(test, pv, nv), replace(ifTrue, pv, nv), otherwise.map(replace(_, pv, nv)))
      
      case NumAdd(left, right) => NumAdd(replace(left, pv, nv), replace(right, pv, nv))
      
      case NumSub(left, right) => NumSub(replace(left, pv, nv), replace(right, pv, nv))
      
      case NumLess(left, right) => NumLess(replace(left, pv, nv), replace(right, pv, nv))
      
      case NumSum(indexing, integrand) => NumSum(replace(indexing, pv, nv), replace(integrand, pv, nv))
      
      case NumProd(indexing, integrand) => NumProd(replace(indexing, pv, nv), replace(integrand, pv, nv))
      
      case NumMax(indexing, integrand) => NumMax(replace(indexing, pv, nv), replace(integrand, pv, nv))
      
      case NumMin(indexing, integrand) => NumMin(replace(indexing, pv, nv), replace(integrand, pv, nv))
      
      case NumMult(left, right) => NumMult(replace(left, pv, nv), replace(right, pv, nv))
      
      case NumDiv(left, right) => NumDiv(replace(left, pv, nv), replace(right, pv, nv))
      
      case NumDivExact(left, right) => NumDivExact(replace(left, pv, nv), replace(right, pv, nv))
      
      case NumMod(left, right) => NumMod(replace(left, pv, nv), replace(right, pv, nv))
      
      case NumUnaryPlus(expr) => NumUnaryPlus(replace(expr, pv, nv))
      
      case NumUnaryMinus(expr) => NumUnaryMinus(replace(expr, pv, nv))
      
      case NumRaise(left, right) => NumRaise(replace(left, pv, nv), replace(right, pv, nv))
      
      case x: ParamRef => replace(x, pv, nv)
      
      case x: DummyIndRef => replace(x, pv, nv)
      
      case x: NumFuncRef => replace(x, pv, nv)
      
      case NumLit(num) => NumLit(replace(num, pv, nv))
    }
  }
  
  implicit val NumFuncRefReplace: Replace[NumFuncRef] = new Replace[NumFuncRef] {
    def rep[B](x: NumFuncRef, pv: B, nv: B): NumFuncRef = rep_(x, pv, nv) {
      case Abs(expr) => Abs(replace(expr, pv, nv))
      
      case Atan(expr) => Atan(replace(expr, pv, nv))
      
      case Atan2(x1, x2) => Atan2(replace(x1, pv, nv), replace(x2, pv, nv))
      
      case Card(expr) => Card(replace(expr, pv, nv))
      
      case Ceil(expr) => Ceil(replace(expr, pv, nv))
      
      case Cos(expr) => Cos(replace(expr, pv, nv))
      
      case Exp(expr) => Exp(replace(expr, pv, nv))
      
      case Floor(expr) => Floor(replace(expr, pv, nv))
      
      case Gmtime() => x
      
      case Length(expr) => Length(replace(expr, pv, nv))
      
      case Log(expr) => Log(replace(expr, pv, nv))
      
      case Log10(expr) => Log10(replace(expr, pv, nv))
      
      case Max(expr @ _*) => Max(expr.map(replace(_, pv, nv)): _*)
      
      case Min(expr @ _*) => Min(expr.map(replace(_, pv, nv)): _*)
      
      case Round(expr, n) => Round(replace(expr, pv, nv), n.map(replace(_, pv, nv)))
      
      case Sin(expr) => Sin(replace(expr, pv, nv))
      
      case Sqrt(expr) => Sqrt(replace(expr, pv, nv))
      
      case Str2time(s, f) => Str2time(replace(s, pv, nv), replace(f, pv, nv))
      
      case Trunc(expr, n) => Trunc(replace(expr, pv, nv), n.map(replace(_, pv, nv)))
      
      case Irand224() => x
      
      case Uniform01() => x
    }
  }
  
  // SYMBOLIC
  implicit val SymExprReplace: Replace[SymExpr] = new Replace[SymExpr] {
    def rep[B](x: SymExpr, pv: B, nv: B): SymExpr = rep_(x, pv, nv) {
      case CondSymExpr(test, ifTrue, otherwise) =>
        CondSymExpr(replace(test, pv, nv), replace(ifTrue, pv, nv), otherwise.map(replace(_, pv, nv)))
      
      case Concat(left, right) => Concat(replace(left, pv, nv), replace(right, pv, nv))
      
      case SymNumExpr(expr) => SymNumExpr(replace(expr, pv, nv))
      
      case x: ParamRef => replace(x, pv, nv)
      
      case x: DummyIndRef => replace(x, pv, nv)
      
      case x: SymFuncRef => replace(x, pv, nv)
      
      case x: StringLit => replace(x, pv, nv)
    }
  }
  
  implicit val SymFuncRefReplace: Replace[SymFuncRef] = new Replace[SymFuncRef] {
    def rep[B](x: SymFuncRef, pv: B, nv: B): SymFuncRef = rep_(x, pv, nv) {
      case Substr(expr, from, length) =>
        Substr(replace(expr, pv, nv), replace(from, pv, nv), length.map(replace(_, pv, nv)))
      
      case Time2str(t, f) => Time2str(replace(t, pv, nv), replace(f, pv, nv))
    }
  }
  
  // SET
  implicit val SetExprReplace: Replace[SetExpr] = new Replace[SetExpr] {
    def rep[B](x: SetExpr, pv: B, nv: B): SetExpr = rep_(x, pv, nv) {
      case CondSetExpr(test, ifTrue, otherwise) =>
      CondSetExpr(replace(test, pv, nv), replace(ifTrue, pv, nv), replace(otherwise, pv, nv))
      
      case Union(left, right) => Union(replace(left, pv, nv), replace(right, pv, nv))
      
      case Diff(left, right) => Diff(replace(left, pv, nv), replace(right, pv, nv))
      
      case SymDiff(left, right) => SymDiff(replace(left, pv, nv), replace(right, pv, nv))
      
      case Inter(left, right) => Inter(replace(left, pv, nv), replace(right, pv, nv))
      
      case Cross(left, right) => Cross(replace(left, pv, nv), replace(right, pv, nv))
      
      case SetOf(indexing, integrand) => SetOf(replace(indexing, pv, nv), integrand.map(replace(_, pv, nv)))
      
      case ArithSet(t0, tf, deltaT) => ArithSet(replace(t0, pv, nv), replace(tf, pv, nv), deltaT.map(replace(_, pv, nv)))
      
      case SetRef(set, subscript) => SetRef(replace(set, pv, nv), subscript.map(replace(_, pv, nv)))
      
      case SetLit(values @ _*) => SetLit(values.map(_.map(replace(_, pv, nv))): _*)
      
      case IndExprSet(indexing) => IndExprSet(replace(indexing, pv, nv))
    }
  }
  
  // INDEXING
  implicit val IndExprReplace: Replace[IndExpr] = new Replace[IndExpr] {
    def rep[B](x: IndExpr, pv: B, nv: B): IndExpr = rep_(x, pv, nv) {
      case IndExpr(entries, predicate) => IndExpr(entries.map(replace(_, pv, nv)), predicate.map(replace(_, pv, nv)))
    }
  }
  
  implicit val IndEntryReplace: Replace[IndEntry] = new Replace[IndEntry] {
    def rep[B](x: IndEntry, pv: B, nv: B): IndEntry = rep_(x, pv, nv) {
      case IndEntry(indices, expr, filter) => IndEntry(indices.map(replace(_, pv, nv)), replace(expr, pv, nv), filter.map(replace(_, pv, nv)))
    }
  }
  
  implicit val DummyIndDeclReplace: Replace[DummyIndDecl] = new Replace[DummyIndDecl] {
    def rep[B](x: DummyIndDecl, pv: B, nv: B): DummyIndDecl = rep_(x, pv, nv) {
      case DummyIndDecl(name, synthetic) => DummyIndDecl(replace(name, pv, nv), synthetic)
    }
  }
  
  //LOGIC
  implicit val LogicExprReplace: Replace[LogicExpr] = new Replace[LogicExpr] {
    def rep[B](x: LogicExpr, pv: B, nv: B): LogicExpr = rep_(x, pv, nv) {
      case Disj(left, right) => Disj(replace(left, pv, nv), replace(right, pv, nv))
      
      case Forall(indexing, integrand) => Forall(replace(indexing, pv, nv), replace(integrand, pv, nv))
      
      case Exists(indexing, integrand) => Exists(replace(indexing, pv, nv), replace(integrand, pv, nv))
      
      case Conj(left, right) => Conj(replace(left, pv, nv), replace(right, pv, nv))
      
      case Neg(expr) => Neg(replace(expr, pv, nv))
      
      case LT(left, right) => LT(replace(left, pv, nv), replace(right, pv, nv))
      
      case LTE(left, right) => LTE(replace(left, pv, nv), replace(right, pv, nv))
      
      case GT(left, right) => GT(replace(left, pv, nv), replace(right, pv, nv))
      
      case GTE(left, right) => GTE(replace(left, pv, nv), replace(right, pv, nv))
      
      case NEq(left, right) => NEq(replace(left, pv, nv), replace(right, pv, nv))
      
      case Eq(left, right) => Eq(replace(left, pv, nv), replace(right, pv, nv))
      
      case NotIn(values, set) => NotIn(values.map(replace(_, pv, nv)), replace(set, pv, nv))
      
      case In(values, set) => In(values.map(replace(_, pv, nv)), replace(set, pv, nv))
      
      case NotWithin(left, right) => NotWithin(replace(left, pv, nv), replace(right, pv, nv))
      
      case Within(left, right) => Within(replace(left, pv, nv), replace(right, pv, nv))
      
      case x: NumExpr => replace(x, pv, nv)
    }
  }
  
  // LINEAR
  implicit val LinExprReplace: Replace[LinExpr] = new Replace[LinExpr] {
    def rep[B](x: LinExpr, pv: B, nv: B): LinExpr = rep_(x, pv, nv) {
      case CondLinExpr(test, ifTrue, otherwise) =>
        CondLinExpr(replace(test, pv, nv), replace(ifTrue, pv, nv), otherwise.map(replace(_, pv, nv)))
      
      case LinAdd(left, right) => LinAdd(replace(left, pv, nv), replace(right, pv, nv))
      
      case LinSub(left, right) => LinSub(replace(left, pv, nv), replace(right, pv, nv))
      
      case LinSum(indexing, integrand) => LinSum(replace(indexing, pv, nv), replace(integrand, pv, nv))
      case LinSumExp(summands) => LinSumExp(summands.map(replace(_, pv, nv)))
      
      case LinMult(left, right) => LinMult(replace(left, pv, nv), replace(right, pv, nv))
      
      case LinDiv(left, right) => LinDiv(replace(left, pv, nv), replace(right, pv, nv))
      
      case LinUnaryPlus(expr) => LinUnaryPlus(replace(expr, pv, nv))
      
      case LinUnaryMinus(expr) => LinUnaryMinus(replace(expr, pv, nv))
      
      case VarRef(xvar, subscript) => VarRef(replace(xvar, pv, nv), subscript.map(replace(_, pv, nv)))
      
      case x: NumExpr => replace(x, pv, nv)
    }
  }
  
  // BASIC
  
  // custom instances of `replace' because it is used directly in references
  implicit val StringLitReplace: Replace[StringLit] = new Replace[StringLit] {
    def rep[B](x: StringLit, pv: B, nv: B): StringLit = rep_(x, pv, nv) {
      case StringLit(str) => StringLit(replace(str, pv, nv))
    }
  }
  
  implicit val BigDecimalReplace: Replace[BigDecimal] = new Replace[BigDecimal] {
    def rep[B](x: BigDecimal, pv: B, nv: B): BigDecimal = rep_(x, pv, nv)(_ => x)
  }
  
  implicit val StringReplace: Replace[String] = new Replace[String] {
    def rep[B](x: String, pv: B, nv: B): String = rep_(x, pv, nv)(_ => x)
  }
  
  implicit val IntReplace: Replace[Int] = new Replace[Int] {
    def rep[B](x: Int, pv: B, nv: B): Int = rep_(x, pv, nv)(_ => x)
  }
}