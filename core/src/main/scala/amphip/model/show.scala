package amphip.model

import cats.Show
import cats.instances.all._
import cats.syntax.show._
import cats.syntax.list._
import cats.syntax.either._
import cats.syntax.option._

import amphip.model.ast._

object show extends ShowInstances

trait ShowInstances {

  // MODEL

  implicit val ModelShow: Show[Model] = Show.show {

    case Model(statements) =>
      val statShow = statements.map(_.show)
      val statsShows = statShow.mkString("\n\n")
      s"$statsShows\n\nend;"

  }

  // STATEMENTS
  implicit val StatShow: Show[Stat] = Show.show {

    case x: SetStat => x.show

    case x: ParamStat => x.show

    case x: VarStat => x.show

    case x: ConstraintStat => x.show

    case x: ObjectiveStat => x.show

  }

  implicit val SetStatShow: Show[SetStat] = Show.show {

    case SetStat(name, alias, domain, atts) =>
      showStat("set", name, alias, domain, atts)

  }

  implicit val ParamStatShow: Show[ParamStat] = Show.show {

    case ParamStat(name, alias, domain, atts) =>
      showStat("param", name, alias, domain, atts)

  }

  implicit val VarStatShow: Show[VarStat] = Show.show {

    case VarStat(name, alias, domain, atts) =>
      showStat("var", name, alias, domain, atts)

  }

  implicit val ConstraintStatShow: Show[ConstraintStat] = Show.show {

    case EqConstraintStat(name, alias, domain, left, right) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${left.show} == ${right.show};"

    case LTEConstraintStat(name, alias, domain, left, right) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${left.show} <= ${right.show};"

    case GTEConstraintStat(name, alias, domain, left, right) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${left.show} >= ${right.show};"

    case DGTEConstraintStat(name, alias, domain, upper, expr, lower) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${upper.show} >= ${expr.show} >= ${lower.show};"

    case DLTEConstraintStat(name, alias, domain, lower, expr, upper) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${lower.show} <= ${expr.show} <= ${upper.show};"

  }

  implicit val ObjectiveStatShow: Show[ObjectiveStat] = Show.show {

    case Minimize(name, alias, domain, expr) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"minimize $nameAliasDomainShows: ${expr.show};"

    case Maximize(name, alias, domain, expr) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"maximize $nameAliasDomainShows: ${expr.show};"

  }

  implicit def SetAttShow: Show[SetAtt] = Show.show {

    case SetDimen(n) =>
      s"dimen ${n.show}"

    case SetWithin(expr) =>
      s"within ${expr.show}"

    case SetAssign(expr) =>
      s":= ${expr.show}"

    case SetDefault(expr) =>
      s"default ${expr.show}"

  }

  implicit def ParamAttShow: Show[ParamAtt] = Show.show {

    case ParamLT(expr) => s"< ${expr.show}"

    case ParamLTE(expr) => s"<= ${expr.show}"

    case ParamEq(expr) => s"= ${expr.show}"

    case ParamNEq(expr) => s"<> ${expr.show}"

    case ParamGT(expr) => s"> ${expr.show}"

    case ParamGTE(expr) => s">= ${expr.show}"

    case ParamIn(expr) => s"in ${expr.show}"

    case ParamAssign(expr) => s":= ${expr.show}"

    case ParamDefault(expr) => s"default ${expr.show}"

    case Integer => "integer"

    case Binary => "binary"

    case Symbolic => "symbolic"

  }

  implicit def VarAttShow: Show[VarAtt] = Show.show {

    case VarLTE(expr) => s"<= ${expr.show}"

    case VarEq(expr) => s"= ${expr.show}"

    case VarGTE(expr) => s">= ${expr.show}"

    case Integer => "integer"

    case Binary => "binary"

  }

  // EXPRESSIONS
  implicit val ExprShow: Show[Expr] = Show.show {

    case x: SimpleExpr => x.show

    case x: SetExpr => x.show

    case x: LogicExpr => x.show

    case x: LinExpr => x.show

  }

  // SIMPLE
  implicit val SimpleExprShow: Show[SimpleExpr] = Show.show {

    case x: NumExpr => x.show

    case x: SymExpr => x.show

  }

  implicit val NumExprWithSymExprShow: Show[NumExpr with SymExpr] = Show.show {

    case x: ParamRef => x.show

    case x: DummyIndRef => x.show

  }

  implicit val ParamRefShow: Show[ParamRef] = Show.show {

    case ParamRef(param, subscript) =>
      val nameShows = param.name
      val subscriptShows = showSubscript(subscript)
      s"$nameShows$subscriptShows"

  }

  implicit val DummyIndRefShow: Show[DummyIndRef] = Show.show {

    case DummyIndRef(dummyInd) => dummyInd.name

  }

  // NUMERIC
  implicit val NumExprShow: Show[NumExpr] = Show.show {

    case CondNumExpr(test, ifTrue, otherwise) =>
      val otherwiseShows = otherwise.fold(")")(expr => s" else ${expr.show})")
      s"(if ${test.show} then ${ifTrue.show}$otherwiseShows"

    case NumAdd(left, right) => s"(${left.show} + ${right.show})"

    case NumSub(left, right) => s"(${left.show} - ${right.show})"

    case NumLess(left, right) => s"(${left.show} less ${right.show})"

    case NumSum(indexing, integrand) => s"(sum${indexing.show} ${integrand.show})"

    case NumProd(indexing, integrand) => s"(prod${indexing.show} ${integrand.show})"

    case NumMax(indexing, integrand) => s"(max${indexing.show} ${integrand.show})"

    case NumMin(indexing, integrand) => s"(min${indexing.show} ${integrand.show})"

    case NumMult(left, right) => s"(${left.show} * ${right.show})"

    case NumDiv(left, right) => s"(${left.show} / ${right.show})"

    case NumDivExact(left, right) => s"(${left.show} div ${right.show})"

    case NumMod(left, right) => s"(${left.show} mod ${right.show})"

    case NumUnaryPlus(x) => s"(+${x.show})"

    case NumUnaryMinus(x) => s"(-${x.show})"

    case NumRaise(left, right) => s"(${left.show} ** ${right.show})"

    case x: ParamRef => x.show

    case x: DummyIndRef => x.show

    case x: NumFuncRef => x.show

    case NumLit(num) => num.show

  }

  implicit val NumFuncRefShow: Show[NumFuncRef] = Show.show {

    case Abs(x) => s"abs(${x.show})"

    case Atan(x) => s"atan(${x.show})"

    case Atan2(x1, x2) => s"atan(${x1.show}, ${x2.show})"

    case Card(x) => s"card(${x.show})"

    case Ceil(x) => s"ceil(${x.show})"

    case Cos(x) => s"cos(${x.show})"

    case Exp(x) => s"exp(${x.show})"

    case Floor(x) => s"floor(${x.show})"

    case Gmtime() => s"gmtime()"

    case Length(x) => s"length(${x.show})"

    case Log(x) => s"log(${x.show})"

    case Log10(x) => s"log10(${x.show})"

    case Max(xs @ _*) => s"max(${xs.map(_.show).mkString(", ")})"

    case Min(xs @ _*) => s"min(${xs.map(_.show).mkString(", ")})"

    case Round(x, n) =>
      val nShows = n.fold("")(x => s", ${x.show}")
      s"round(${x.show}$nShows)"

    case Sin(x) => s"sin(${x.show})"

    case Sqrt(x) => s"sqrt(${x.show})"

    case Str2time(s, f) => s"str2time(${s.show}, ${f.show})"

    case Trunc(x, n) =>
      val nShows = n.fold("")(x => s", ${x.show}")
      s"trunc(${x.show}$nShows)"

    case Irand224() => s"Irand224()"

    case Uniform01() => s"Uniform01()"

  }

  // SYMBOLIC
  implicit val SymExprShow: Show[SymExpr] = Show.show {

    case CondSymExpr(test, ifTrue, otherwise) =>
      val otherwiseShows = otherwise.fold(")")(expr => s" else ${expr.show})")
      s"(if ${test.show} then ${ifTrue.show}$otherwiseShows"

    case Concat(left, right) => s"(${left.show} & ${right.show})"

    case SymNumExpr(expr) => expr.show

    case x: ParamRef => x.show

    case x: DummyIndRef => x.show

    case x: SymFuncRef => x.show

    case x: StringLit => s""""${x.str.replaceAll("\"", "\"\"").show}""""

  }

  implicit val SymFuncRefShow: Show[SymFuncRef] = Show.show {

    case Substr(x, from, length) =>
      val lengthShows = length.fold("")(x => s", ${x.show}")
      s"substr(${x.show}, ${from.show}$lengthShows)"

    case Time2str(t, f) => s"time2str(${t.show}, ${f.show})"

  }

  // SET
  implicit val SetExprShow: Show[SetExpr] = Show.show {

    case CondSetExpr(test, ifTrue, otherwise) =>
      s"(if ${test.show} then ${ifTrue.show} else ${otherwise.show})"

    case Union(expr1, expr2) => s"(${expr1.show} union ${expr2.show})"

    case Diff(expr1, expr2) => s"(${expr1.show} diff ${expr2.show})"

    case SymDiff(expr1, expr2) => s"(${expr1.show} symdiff ${expr2.show})"

    case Inter(expr1, expr2) => s"(${expr1.show} inter ${expr2.show})"

    case Cross(expr1, expr2) => s"(${expr1.show} cross ${expr2.show})"

    case SetOf(indexing, integrand) =>
      val indexingShows = indexing.show
      val integrandShows = showTuple(integrand)

      s"(setof$indexingShows $integrandShows)"

    case ArithSet(t0, tf, deltaT) =>
      val t0Shows = t0.show
      val tfShows = tf.show
      val deltaTShows = deltaT.fold("")(expr => s" by ${expr.show}")
      s"$t0Shows .. $tfShows$deltaTShows"

    case x: SetRef => x.show

    case SetLit(values @ _*) =>
      val valuesShow = values.map(showTuple).mkString(", ")

      s"{$valuesShow}"

    case IndExprSet(indexing) => indexing.show

  }

  implicit val SetRefShow: Show[SetRef] = Show.show {
    case SetRef(set, subscript) =>
      val nameShows = set.name
      val subscriptShows = showSubscript(subscript)
      s"$nameShows$subscriptShows"
  }

  //INDEXING
  implicit val IndExprShow: Show[IndExpr] = Show.show {

    case IndExpr(entries, predicate) =>
      val entriesShows = entries.map(_.show).mkString(", ")
      val predicateShows = predicate.fold("")(x => s" : ${x.show}")
      s"{$entriesShows$predicateShows}"

  }

  implicit val IndEntryShow: Show[IndEntry] = Show.show {

    case IndEntry(Nil, expr, _) =>
      expr.show

    case IndEntry(indices, expr, None) =>
      val indicesShows = showTupleInd(indices)
      s"$indicesShows in ${expr.show}"

    case IndEntry(indices, expr, Some(predicate)) =>

      val map            = IndEntry.extract(predicate)
      val simplifiedPred = IndEntry.simplify(predicate)
      val predicateShows = simplifiedPred.fold("")(x => s" : ${x.show}")

      val indExpr = indices.map { x => map.get(x).toLeft(x) }
      val indicesShows = showTupleDisj(indExpr)

      s"$indicesShows in ${expr.show}$predicateShows"

  }

  implicit val DummyIndDeclShow: Show[DummyIndDecl] = Show.show {

    case DummyIndDecl(name, _) => name

  }

  // LOGIC
  implicit def LogicExprShow: Show[LogicExpr] = Show.show {

    case Disj(left, right) =>
      s"(${left.show} or ${right.show})"

    case Forall(indexing: IndExpr, integrand: LogicExpr) =>
      s"(forall${indexing.show} ${integrand.show})"

    case Exists(indexing: IndExpr, integrand: LogicExpr) =>
      s"(exists${indexing.show} ${integrand.show})"

    case Conj(left, right) =>
      s"(${left.show} and ${right.show})"

    case Neg(x: LogicExpr) =>
      s"(not ${x.show})"

    case LT(left, right) =>
      s"(${left.show} < ${right.show})"

    case LTE(left, right) =>
      s"(${left.show} <= ${right.show})"

    case GT(left, right) =>
      s"(${left.show} > ${right.show})"

    case GTE(left, right) =>
      s"(${left.show} >= ${right.show})"

    case NEq(left, right) =>
      s"(${left.show} <> ${right.show})"

    case Eq(left, right) =>
      s"(${left.show} == ${right.show})"

    case NotIn(values, set) =>
      val valuesShow = showTuple(values)
      s"($valuesShow not in ${set.show})"

    case In(values, set) =>
      val valuesShow = showTuple(values)
      s"($valuesShow in ${set.show})"

    case NotWithin(left, right) =>
      s"(${left.show} not within ${right.show})"

    case Within(left, right) =>
      s"(${left.show} within ${right.show})"

    case x: NumExpr => x.show

  }

  // LINEAR
  implicit val LinExprShow: Show[LinExpr] = Show.show {

    case CondLinExpr(test, ifTrue, otherwise) =>
      val otherwiseShows = otherwise.fold(")")(expr => s" else ${expr.show})")
      s"(if ${test.show} then ${ifTrue.show}$otherwiseShows"

    case LinAdd(left, right) => s"(${left.show} + ${right.show})"

    case LinSub(left, right) => s"(${left.show} - ${right.show})"

    case LinSum(indexing, integrand) => s"(sum${indexing.show} ${integrand.show})"
    case LinSumExp(summands) => s"(${summands.map(_.show).mkString(" + ")})"

    case LinMult(left, right) => s"(${left.show} * ${right.show})"

    case LinDiv(left, right) => s"(${left.show} / ${right.show})"

    case LinUnaryPlus(x) => s"(+${x.show})"

    case LinUnaryMinus(x) => s"(-${x.show})"

    case x: VarRef => x.show

    case x: NumExpr => x.show

  }

  implicit val VarRefShow: Show[VarRef] = Show.show {

    case VarRef(xvar, subscript) =>
      val nameShows = xvar.name
      val subscriptShows = showSubscript(subscript)
      s"$nameShows$subscriptShows"

  }

  // BASIC 

  private[this] def showStat(name: SymName, alias: Option[StringLit], domain: Option[IndExpr]): String = {
    val nameShows = name.some
    val aliasShows = alias.map(x => (x: SymExpr).show)
    val domainShows = domain.map(_.show)

    if (alias.isDefined) {
      Seq(nameShows, aliasShows, domainShows).flatten.mkString(" ")
    } else {
      Seq(nameShows, domainShows).flatten.mkString("")
    }
  }

  private[this] def showStat[T: Show](stat: String, name: SymName, alias: Option[StringLit], domain: Option[IndExpr], atts: List[T]): String = {
    val nameAliasDomainShows = showStat(name, alias, domain)
    val attsShows = atts.map(_.show).toNel.map(_.toList.mkString(", "))
    val nameAliasDomainAttsShows = Seq(nameAliasDomainShows.some, attsShows).flatten.mkString(" ")

    s"$stat $nameAliasDomainAttsShows;"
  }

  private[this] def showSubscript(subscript: List[SimpleExpr]): String = {
    subscript.toNel.fold("") { list => "[" + list.map(_.show).toList.mkString(", ") + "]" }
  }

  private[this] def showTupleDisj(tuple: List[Either[SimpleExpr, DummyIndDecl]]): String = {
    def show(x: Either[SimpleExpr, DummyIndDecl]) = x match {
      case Left(a) => a.show
      case Right(b) => b.show
    }

    if (tuple.size == 1)
      show(tuple(0))
    else
      tuple.map(show).mkString("(", ", ", ")")
  }

  private[this] def showTuple(tuple: Tuple): String = showTupleDisj(tuple.map(_.asLeft))
  private[this] def showTupleInd(tuple: List[DummyIndDecl]): String = showTupleDisj(tuple.map(_.asRight))

}