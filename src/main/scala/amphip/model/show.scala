package amphip.model

import scalaz.{ \/, -\/, \/-, Scalaz }, Scalaz._
import scalaz.Show, Show._

import amphip.model.ast._

object show extends ShowInstances

trait ShowInstances {

  // MODEL

  implicit val ModelShow: Show[Model] = shows {

    case Model(statements) =>
      val statShow = statements.map(_.shows)
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

  implicit val SetStatShow: Show[SetStat] = shows {

    case SetStat(name, alias, domain, atts) =>
      showStat("set", name, alias, domain, atts)

  }

  implicit val ParamStatShow: Show[ParamStat] = shows {

    case ParamStat(name, alias, domain, atts) =>
      showStat("param", name, alias, domain, atts)

  }

  implicit val VarStatShow: Show[VarStat] = shows {

    case VarStat(name, alias, domain, atts) =>
      showStat("var", name, alias, domain, atts)

  }

  implicit val ConstraintStatShow: Show[ConstraintStat] = shows {

    case EqConstraintStat(name, alias, domain, left, right) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${left.shows} = ${right.shows};"

    case LTEConstraintStat(name, alias, domain, left, right) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${left.shows} <= ${right.shows};"

    case GTEConstraintStat(name, alias, domain, left, right) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${left.shows} >= ${right.shows};"

    case DGTEConstraintStat(name, alias, domain, upper, expr, lower) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${upper.shows} >= ${expr.shows} >= ${lower.shows};"

    case DLTEConstraintStat(name, alias, domain, lower, expr, upper) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"s.t. $nameAliasDomainShows: ${lower.shows} <= ${expr.shows} <= ${upper.shows};"

  }

  implicit val ObjectiveStatShow: Show[ObjectiveStat] = shows {

    case Minimize(name, alias, domain, expr) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"minimize $nameAliasDomainShows: ${expr.shows};"

    case Maximize(name, alias, domain, expr) =>
      val nameAliasDomainShows = showStat(name, alias, domain)
      s"maximize $nameAliasDomainShows: ${expr.shows};"

  }

  implicit def SetAttShow: Show[SetAtt] = shows {

    case SetDimen(n) =>
      s"dimen ${n.shows}"

    case SetWithin(expr) =>
      s"within ${expr.shows}"

    case SetAssign(expr) =>
      s":= ${expr.shows}"

    case SetDefault(expr) =>
      s"default ${expr.shows}"

  }

  implicit def ParamAttShow: Show[ParamAtt] = shows {

    case ParamLT(expr) => s"< ${expr.shows}"

    case ParamLTE(expr) => s"<= ${expr.shows}"

    case ParamEq(expr) => s"= ${expr.shows}"

    case ParamNEq(expr) => s"<> ${expr.shows}"

    case ParamGT(expr) => s"> ${expr.shows}"

    case ParamGTE(expr) => s">= ${expr.shows}"

    case ParamIn(expr) => s"in ${expr.shows}"

    case ParamAssign(expr) => s":= ${expr.shows}"

    case ParamDefault(expr) => s"default ${expr.shows}"

    case Integer => "integer"

    case Binary => "binary"

    case Symbolic => "symbolic"

  }

  implicit def VarAttShow: Show[VarAtt] = shows {

    case VarLTE(expr) => s"<= ${expr.shows}"

    case VarEq(expr) => s"= ${expr.shows}"

    case VarGTE(expr) => s">= ${expr.shows}"

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

  implicit val ParamRefShow: Show[ParamRef] = shows {

    case ParamRef(param, subscript) =>
      val nameShows = param.name
      val subscriptShows = showSubscript(subscript)
      s"$nameShows$subscriptShows"

  }

  implicit val DummyIndRefShow: Show[DummyIndRef] = shows {

    case DummyIndRef(dummyInd) => dummyInd.name

  }

  // NUMERIC
  implicit val NumExprShow: Show[NumExpr] = shows {

    case CondNumExpr(test, ifTrue, otherwise) =>
      val otherwiseShows = otherwise.fold(")")(expr => s" else ${expr.shows})")
      s"(if ${test.shows} then ${ifTrue.shows}$otherwiseShows"

    case NumAdd(left, right) => s"(${left.shows} + ${right.shows})"

    case NumSub(left, right) => s"(${left.shows} - ${right.shows})"

    case NumLess(left, right) => s"(${left.shows} less ${right.shows})"

    case NumSum(indexing, integrand) => s"(sum${indexing.shows} ${integrand.shows})"

    case NumProd(indexing, integrand) => s"(prod${indexing.shows} ${integrand.shows})"

    case NumMax(indexing, integrand) => s"(max${indexing.shows} ${integrand.shows})"

    case NumMin(indexing, integrand) => s"(min${indexing.shows} ${integrand.shows})"

    case NumMult(left, right) => s"(${left.shows} * ${right.shows})"

    case NumDiv(left, right) => s"(${left.shows} / ${right.shows})"

    case NumDivExact(left, right) => s"(${left.shows} div ${right.shows})"

    case NumMod(left, right) => s"(${left.shows} mod ${right.shows})"

    case NumUnaryPlus(x) => s"(+${x.shows})"

    case NumUnaryMinus(x) => s"(-${x.shows})"

    case NumRaise(left, right) => s"(${left.shows} ** ${right.shows})"

    case x: ParamRef => x.shows

    case x: DummyIndRef => x.shows

    case x: NumFuncRef => x.shows

    case x: NumLit => x.num.shows

  }

  implicit val NumFuncRefShow: Show[NumFuncRef] = shows {

    case Abs(x) => s"abs(${x.shows})"

    case Atan(x) => s"atan(${x.shows})"

    case Atan2(x1, x2) => s"atan(${x1.shows}, ${x2.shows})"

    case Card(x) => s"card(${x.shows})"

    case Ceil(x) => s"ceil(${x.shows})"

    case Cos(x) => s"cos(${x.shows})"

    case Exp(x) => s"exp(${x.shows})"

    case Floor(x) => s"floor(${x.shows})"

    case Gmtime() => s"gmtime()"

    case Length(x) => s"length(${x.shows})"

    case Log(x) => s"log(${x.shows})"

    case Log10(x) => s"log10(${x.shows})"

    case Max(x) => s"max(${x.shows})"

    case Min(x) => s"min(${x.shows})"

    case Round(x, n) =>
      val nShows = n.fold("")(x => s", ${x.shows}")
      s"round(${x.shows}$nShows)"

    case Sin(x) => s"sin(${x.shows})"

    case Sqrt(x) => s"sqrt(${x.shows})"

    case Str2time(s, f) => s"str2time(${s.shows}, ${f.shows})"

    case Trunc(x, n) =>
      val nShows = n.fold("")(x => s", ${x.shows}")
      s"trunc(${x.shows}$nShows)"

    case Irand224() => s"Irand224()"

    case Uniform01() => s"Uniform01()"

  }

  // SYMBOLIC
  implicit val SymExprShow: Show[SymExpr] = shows {

    case CondSymExpr(test, ifTrue, otherwise) =>
      val otherwiseShows = otherwise.fold(")")(expr => s" else ${expr.shows})")
      s"(if ${test.shows} then ${ifTrue.shows}$otherwiseShows"

    case Concat(left, right) => s"(${left.shows} & ${right.shows})"

    case SymNumExpr(expr) => expr.shows

    case x: ParamRef => x.shows

    case x: DummyIndRef => x.shows

    case x: SymFuncRef => x.shows

    case x: StringLit => x.str.replaceAll("\"", "\"\"").shows

  }

  implicit val SymFuncRefShow: Show[SymFuncRef] = shows {

    case Substr(x, from, length) =>
      val lengthShows = length.fold("")(x => s", ${x.shows}")
      s"substr(${x.shows}, ${from.shows}$lengthShows)"

    case Time2str(t, f) => s"time2str(${t.shows}, ${f.shows})"

  }

  // SET
  implicit val SetExprShow: Show[SetExpr] = shows {

    case CondSetExpr(test, ifTrue, otherwise) =>
      s"(if ${test.shows} then ${ifTrue.shows} else ${otherwise.shows})"

    case Union(expr1, expr2) => s"(${expr1.shows} union ${expr2.shows})"

    case Diff(expr1, expr2) => s"(${expr1.shows} diff ${expr2.shows})"

    case SymDiff(expr1, expr2) => s"(${expr1.shows} symdiff ${expr2.shows})"

    case Inter(expr1, expr2) => s"(${expr1.shows} inter ${expr2.shows})"

    case Cross(expr1, expr2) => s"(${expr1.shows} cross ${expr2.shows})"

    case SetOf(indexing, integrand) =>
      val indexingShows = indexing.shows
      val integrandShows = showTuple(integrand)

      s"(setof$indexingShows $integrandShows)"

    case ArithSet(t0, tf, deltaT) =>
      val t0Shows = t0.shows
      val tfShows = tf.shows
      val deltaTShows = deltaT.fold("")(expr => s" by ${expr.shows}")
      s"$t0Shows .. $tfShows$deltaTShows"

    case SetRef(set, subscript) =>
      val nameShows = set.name
      val subscriptShows = showSubscript(subscript)
      s"$nameShows$subscriptShows"

    case SetLit(values @ _*) =>
      val valuesShow = values.map(showTuple).mkString(", ")

      s"{$valuesShow}"

    case IndExprSet(indexing) => indexing.shows

  }

  //INDEXING
  implicit val IndExprShow: Show[IndExpr] = shows {

    case IndExpr(entries, predicate) =>
      val entriesShows = entries.map(_.shows).mkString(", ")
      val predicateShows = predicate.fold("")(x => s" : ${x.shows}")
      s"{$entriesShows$predicateShows}"

  }

  implicit val IndEntryShow: Show[IndEntry] = shows {

    case IndEntry(Nil, expr, _) =>
      expr.shows

    case IndEntry(indices, expr, None) =>
      val indicesShows = showTupleInd(indices)
      s"$indicesShows in ${expr.shows}"

    case IndEntry(indices, expr, Some(predicate)) =>

      val map = IndEntry.extract(predicate)

      val indExpr = indices.map { x => map.get(x).toLeftDisjunction(x) }
      val indicesShows = showTupleDisj(indExpr)

      s"$indicesShows in ${expr.shows}"

  }

  implicit val DummyIndDeclShow: Show[DummyIndDecl] = shows {

    case DummyIndDecl(name, _) => name

  }

  // LOGIC
  implicit def LogicExprShow: Show[LogicExpr] = shows {

    case Disj(left, right) =>
      s"(${left.shows} or ${right.shows})"

    case Forall(indexing: IndExpr, integrand: LogicExpr) =>
      s"(forall${indexing.shows} ${integrand.shows})"

    case Exists(indexing: IndExpr, integrand: LogicExpr) =>
      s"(exists${indexing.shows} ${integrand.shows})"

    case Conj(left, right) =>
      s"(${left.shows} and ${right.shows})"

    case Neg(x: LogicExpr) =>
      s"(not ${x.shows})"

    case LT(left, right) =>
      s"(${left.shows} < ${right.shows})"

    case LTE(left, right) =>
      s"(${left.shows} <= ${right.shows})"

    case GT(left, right) =>
      s"(${left.shows} > ${right.shows})"

    case GTE(left, right) =>
      s"(${left.shows} >= ${right.shows})"

    case NEq(left, right) =>
      s"(${left.shows} <> ${right.shows})"

    case Eq(left, right) =>
      s"(${left.shows} = ${right.shows})"

    case NotIn(values, set) =>
      val valuesShow = showTuple(values)
      s"($valuesShow not in ${set.shows})"

    case In(values, set) =>
      val valuesShow = showTuple(values)
      s"($valuesShow in ${set.shows})"

    case NotWithin(left, right) =>
      s"(${left.shows} not within ${right.shows})"

    case Within(left, right) =>
      s"(${left.shows} within ${right.shows})"

    case x: NumExpr => x.shows

  }

  // LINEAR
  implicit val LinExprShow: Show[LinExpr] = shows {

    case CondLinExpr(test, ifTrue, otherwise) =>
      val otherwiseShows = otherwise.fold(")")(expr => s" else ${expr.shows})")
      s"(if ${test.shows} then ${ifTrue.shows}$otherwiseShows"

    case LinAdd(left, right) => s"(${left.shows} + ${right.shows})"

    case LinSub(left, right) => s"(${left.shows} - ${right.shows})"

    case LinSum(indexing, integrand) => s"(sum${indexing.shows} ${integrand.shows})"

    case LinMult(left, right) => s"(${left.shows} * ${right.shows})"

    case LinDiv(left, right) => s"(${left.shows} / ${right.shows})"

    case LinUnaryPlus(x) => s"(+${x.shows})"

    case LinUnaryMinus(x) => s"(-${x.shows})"

    case x: VarRef => x.shows

    case x: NumExpr => x.shows

  }

  implicit val VarRefShow: Show[VarRef] = shows {

    case VarRef(xvar, subscript) =>
      val nameShows = xvar.name
      val subscriptShows = showSubscript(subscript)
      s"$nameShows$subscriptShows"

  }

  // BASIC 

  private[this] def showStat(name: SymName, alias: Option[StringLit], domain: Option[IndExpr]): String = {
    val nameShows = name.some
    val aliasShows = alias.map(x => (x: SymExpr).shows)
    val domainShows = domain.map(_.shows)

    if (alias.isDefined) {
      Seq(nameShows, aliasShows, domainShows).flatten.mkString(" ")
    } else {
      Seq(nameShows, domainShows).flatten.mkString("")
    }
  }

  private[this] def showStat[T: Show](stat: String, name: SymName, alias: Option[StringLit], domain: Option[IndExpr], atts: List[T]): String = {
    val nameAliasDomainShows = showStat(name, alias, domain)
    val attsShows = atts.map(_.shows).toNel.map(_.list.mkString(", "))
    val nameAliasDomainAttsShows = Seq(nameAliasDomainShows.some, attsShows).flatten.mkString(" ")

    s"$stat $nameAliasDomainAttsShows;"
  }

  private[this] def showSubscript(subscript: List[SimpleExpr]): String = {
    subscript.toNel.fold("") { list => "[" + list.map(_.shows).list.mkString(", ") + "]" }
  }

  private[this] def showTupleDisj(tuple: List[SimpleExpr \/ DummyIndDecl]): String = {
    def shows(x: SimpleExpr \/ DummyIndDecl) = x match {
      case -\/(a) => a.shows
      case \/-(b) => b.shows
    }

    if (tuple.size == 1)
      shows(tuple(0))
    else
      tuple.map(shows).mkString("(", ", ", ")")
  }

  private[this] def showTuple(tuple: Tuple): String = showTupleDisj(tuple.map(_.left))
  private[this] def showTupleInd(tuple: List[DummyIndDecl]): String = showTupleDisj(tuple.map(_.right))

}