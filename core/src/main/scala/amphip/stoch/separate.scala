package amphip.stoch

import cats.syntax.list._
import cats.syntax.option._
import mouse.option._

import amphip.dsl._
import amphip.model.ast._
import amphip.model.{collect, replace}
import amphip.data._
import collect.Collect
import replace.Replace

object separate {

  /**
   * Creates a new model modifying all the deterministic variables,
   * and all other statements referring to these variables, to accept
   * `S' as an indexing set.
   * The returned model does not have any--automatically added--
   * nonanticipativity constraint.
   *
   * This allows to find the WS value.
   *
   * The algorithm used is pretty naive and doesn't handle, among others, the
   * case where the scenarios set, S, is used as part of the expression instead
   * of as a part of the domain.
   * For example:
   * ```
   * s.t. ctr: sum{s in S} (x + y[s]) <= a;
   * ```
   * will be converted to:
   * ```
   * s.t. ctr{s_ in S}: sum{s in S} (x[s_] + y[s]) <= a;
   * ```
   * instead of:
   * ```
   * s.t. ctr: sum{s in S} (x[s] + y[s]) <= a;
   * ```
   */
  def apply(m: StochModel): ModelWithData = {

    val detVarsNames = m.deterministicVariables.map(_.name)

    val m1 = detVarsNames.foldLeft(m) { (m, name) => separateDetVar(m, name) }

    val obj    = m1.objective
    val newObj = separateObjective(detVarsNames, m1.S, m1.pi, obj)
    val m2     = m1.replace(obj, newObj)

    val m2mip    = m2.mip // to generate stochastic data
    // final model must contain the (possible) adaptations made to generate the mip model
    // but not the nonanticipativity contraints
    val nactrSet = m2.nonanticipativityConstraints.map(_.name).toSet
    val stmts    = m2mip.statements.filterNot(x => nactrSet(x.name))
    ModelWithData(Model(stmts), m2mip.data)
  }

  /**
   * Separates the deterministic variable with the specified name and 
   * all the referencing constraints.
   * The objective statement is only modified to replace the var with
   * the separated one.
   */
  def separateDetVar(m: StochModel, name: String): StochModel = {
    val detVar = m.xvar(name)

    val (_, sepVar) = separateVar(m.S, detVar)
    val m1 = m.replace(detVar, sepVar)
    val varName = sepVar.name

    val replacements =
      m1.statements.collect {
        case x: ConstraintStat if references(varName, x).nonEmpty =>
          val (s, newCtr) = separateConstraint(m1.S, x)
          x -> fixSubscript(varName, s, newCtr)
      }

    replacements.foldLeft(m1) {
      case (m1, (stat, newStat)) =>
        m1.replace(stat, newStat)
    }
  }

  def separateVar(S: SetStat, xvar: VarStat): (DummyIndDecl, VarStat) = {
    val (s, newDomain) = separateDomain(S, xvar.domain)
    s -> xvar.copy(domain = newDomain.some)
  }

  def separateConstraint(S: SetStat, ctr: ConstraintStat): (DummyIndDecl, ConstraintStat) = {
    ctr match {
      case x @ EqConstraintStat(_, _, domain, _, _) =>
        val (s, newDomain) = separateDomain(S, domain)
        s -> x.copy(domain = newDomain.some)
      case x @ LTEConstraintStat(_, _, domain, _, _) =>
        val (s, newDomain) = separateDomain(S, domain)
        s -> x.copy(domain = newDomain.some)
      case x @ GTEConstraintStat(_, _, domain, _, _) =>
        val (s, newDomain) = separateDomain(S, domain)
        s -> x.copy(domain = newDomain.some)
      case x @ DGTEConstraintStat(_, _, domain, _, _, _) =>
        val (s, newDomain) = separateDomain(S, domain)
        s -> x.copy(domain = newDomain.some)
      case x @ DLTEConstraintStat(_, _, domain, _, _, _) =>
        val (s, newDomain) = separateDomain(S, domain)
        s -> x.copy(domain = newDomain.some)
    }
  }

  def separateObjective(detVarsNames: List[String], S: SetStat, pi: ParamStat, o: ObjectiveStat): ObjectiveStat = o match {
    case x: Minimize => x.copy(expr = separateLinExpr(detVarsNames, S, pi, x.expr))
    case x: Maximize => x.copy(expr = separateLinExpr(detVarsNames, S, pi, x.expr))
  }

  def separateDomain(S: SetStat, domain: Option[IndExpr]): (DummyIndDecl, IndExpr) = {
    val gen = newGen
    val s = dummy(gen.dummy("s").freshName)

    val SExpr = S()

    domain.cata(
      { indexing =>
        val newEntries = indexing.entries.map {
          case entry @ IndEntry(indices, set, _) if set == SExpr =>
            entry.copy(indices = indices.toNel.map(_.toList).getOrElse(List(s)))
          case x => x
        }

        val newIndexing = indexing.copy(entries = newEntries)

        newIndexing.entries.find(_.set == SExpr).cata(
          {
            case IndEntry(ss :: _, _, _) => // the entry is non-empty in `newEntries'
              ss -> newIndexing
            case x => sys.error(s"""Found `$x' expecting nonEmpty entries list. This is a bug.""")
          },
          s -> newIndexing.copy(entries = (s in S) :: indexing.entries))
      },
      s -> ind(s in S))
  }

  def separateLinExpr(detVarsNames: List[String], S: SetStat, pi: ParamStat, expr: LinExpr): LinExpr = {
    val SExpr = S()
    val comps = summands(expr)
    val (stoch, det) = comps.partition {
      case LinSum(IndExpr(entries, _), _) => entries.exists(_.set == SExpr)
      case _ => false
    }
    val fixedStoch = stoch.flatMap {
      case stochSum @ LinSum(IndExpr(entries, _), _) =>
        val dummyOpt = entries.find(_.set == SExpr).flatMap(_.indices.headOption)
        dummyOpt.map { s =>
          detVarsNames.foldLeft(stochSum: LinExpr) { (stochSum, name) =>
            fixSubscript(name, s, stochSum)
          }
        }
      case x => x.some // never executes, there is only one case in this partition but Scala can't figure it
    }
    val optDetSum = det.toNel.map(_.reduceLeft((left, right) => LinAdd(left, right)))
    val optStochSum = fixedStoch.toNel.map(_.reduceLeft((left, right) => LinAdd(left, right)))
    val newExpr =
      optDetSum.cata(
        { detSum =>
          val gen = newGen
          val s = dummy(gen.dummy("s").freshName)
          val newDetSum = detVarsNames.foldLeft(detSum)((detSum, name) => fixSubscript(name, s, detSum))
          optStochSum.cata(
            stochSum => sum(s in S)(pi(s) * newDetSum) + stochSum,
            sum(s in S)(pi(s) * newDetSum))
        },
        optStochSum.getOrElse(expr))

    newExpr
  }

  def summands(expr: LinExpr): List[LinExpr] = expr match {
    case LinAdd(left, right) => summands(left) ++ summands(right)
    case LinSub(left, right) => summands(left) ++ summands(right)
    case x => List(x)
  }

  def references[A: Collect](varName: String, a: A): List[VarRef] = {
    collect(a, { case x @ VarRef(VarStat(`varName`, _, _, _), _) => x })
  }

  def fixSubscript[A: Collect: Replace](varName: String, s: DummyIndDecl, a: A): A = {
    val refs = references(varName, a).filter(!_.subscript.contains(s: SimpleExpr))

    val newStat = refs.foldLeft(a) { (a, r) =>
      val rr = r.copy(subscript = s :: r.subscript)
      replace(a, r, rr)
    }

    newStat
  }
}
