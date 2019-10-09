package amphip.stoch

import scalaz._, Scalaz._

import spire.math
import spire.implicits._

import amphip.dsl._
import amphip.model.ast._
import amphip.model.dimen

object nonanticipativity {

  /* hint to use as name of the dummy index */
  def nameHint(expr: SetExpr): Option[SymName] = expr match {
    case SetRef(SetStat(name, _, _, _), _) => Some(name.toLowerCase)
    case _ => None
  }

  /* juggling to avoid name colisions */
  def maxStartingWith(subscript: List[DummyIndDecl], start: String, default: String): String = {
    subscript.filter(_.name.startsWith(start)).map(_.name).fold(default)(math.max[String])
  }

  /* calculate the indices needed for each set expression in the entries */
  def assignIndices(entries0: List[IndEntry], T: SetStat, S: SetStat, sIdeal: DummyIndDecl, tIdeal: DummyIndDecl): List[IndEntry] = {
    val gen = newGen

    for {
      entry <- entries0
      indices = entry.indices
    } yield {

      val TExpr = T()
      val SExpr = S()

      val effInd0 =
        if (indices.isEmpty) {
          val dim = dimen(entry.set)
          List.fill(dim)(DummyIndDecl(gen.dummy(nameHint(entry.set)).freshName, synthetic = true))
        } else {
          indices
        }

      val effInd = (entry.set, effInd0) match {
        case (TExpr, List(t)) if t.synthetic => List(tIdeal)
        case (SExpr, List(s)) if s.synthetic => List(sIdeal)
        case _ => effInd0
      }

      IndEntry(effInd, entry.set, entry.predicate)
    }
  }

  def apply(xvar: VarStat, T: SetStat, S: SetStat, link: SetStat): Option[ConstraintStat] = {

    for {
      IndExpr(entries0, predicate) <- xvar.domain
    } yield {

      val TExpr = T()
      val SExpr = S()

      val tIdeal = dummy("t")
      val sIdeal = dummy("s")

      val entries = assignIndices(entries0, T, S, sIdeal, tIdeal)
      val subscript = entries.flatMap(_.indices)
      val t = entries.find(_.set == TExpr).toList.flatMap(_.indices).headOption | tIdeal // `tIdeal' shouldn't be needed here ...
      val s = entries.find(_.set == SExpr).toList.flatMap(_.indices).headOption | sIdeal // `sIdeal' shouldn't be needed here ...

      val s1 = dummy(maxStartingWith(subscript, s"${s.name}1", s.name) + "1")
      val s2 = dummy(maxStartingWith(subscript, s"${s.name}2", s.name) + "2")

      val subscript1 = subscript.map(x => if (x == s) (s + 1) else (x: NumExpr))

      val naPred = s >= s1 && (s + 1) <= s2

      val indexing = IndExpr(
          (t in TExpr) :: 
          ((s1, s2) in link(t)) :: 
          (s in SExpr) :: 
          entries.filterNot(e => List(SExpr, TExpr).contains(e.set)), 
          predicate.cata(naPred && _, naPred).some)

      st(s"NA_${xvar.name}", indexing) {
        xvar(subscript) === xvar(subscript1)
      }
    }
  }

  def apply(xvar: VarStat, T: SetStat, S: SetStat, na: AdaptedNAMode): Option[ConstraintStat] = {

    for {
      IndExpr(entries0, predicate) <- xvar.domain
    } yield {

      val TExpr = T()
      val SExpr = S()

      val tIdeal = dummy("t")
      val sIdeal = dummy("s")

      val entries = assignIndices(entries0, T, S, sIdeal, tIdeal)
      val subscript = entries.flatMap(_.indices)
      val t = entries.find(_.set == TExpr).toList.flatMap(_.indices).headOption | tIdeal // `tIdeal' shouldn't be needed here ...
      val s = entries.find(_.set == SExpr).toList.flatMap(_.indices).headOption | sIdeal // `sIdeal' shouldn't be needed here ...

      val s1 = dummy(maxStartingWith(subscript, s"${s.name}1", s.name) + "1")
      val s2 = dummy(maxStartingWith(subscript, s"${s.name}2", s.name) + "2")

      val subscript1 = subscript.map(x => if (x == s) s1 else x)
      val subscript2 = subscript.map(x => if (x == s) s2 else x)

      val naPred = na.ancf(s1, t) === na.ancf(s2, t)

      val indexing = IndExpr(
          (t in TExpr) :: 
          (s1 in SExpr) :: 
          (s2 in SExpr) :: 
          entries.filterNot(e => List(SExpr, TExpr).contains(e.set)), 
          predicate.cata(naPred && _, naPred).some)

      st(s"NA_${xvar.name}_ctr", indexing) {
        xvar(subscript1) === xvar(subscript2)
      }
    }
  }

  def nonanticipativityTemplate(xvar: VarStat, T: SetStat, S: SetStat, link: ParamStat,
                                func: (VarStat, IndExpr, DummyIndDecl, DummyIndDecl, SetStat, ParamStat, DummyIndDecl, DummyIndDecl, List[DummyIndDecl], List[DummyIndDecl]) => ConstraintStat): Option[ConstraintStat] = {

    for {
      IndExpr(entries0, predicate) <- xvar.domain
    } yield {

      val SExpr = S()
      val TExpr = T()

      val sIdeal = dummy("s")
      val tIdeal = dummy("t")

      /* calculate the indices needed for each set expression in the entries */
      def assignIndices: List[IndEntry] = {
        val gen = newGen

        for {
          entry <- entries0
          indices = entry.indices
        } yield {

          val effInd0 =
            if (indices.isEmpty) {
              val dim = dimen(entry.set)
              List.fill(dim)(DummyIndDecl(gen.dummy(nameHint(entry.set)).freshName, synthetic = true))
            } else {
              indices
            }

          val effInd = (entry.set, effInd0) match {
            case (SExpr, List(s)) if s.synthetic => List(sIdeal)
            case (TExpr, List(t)) if t.synthetic => List(tIdeal)
            case _ => effInd0
          }

          IndEntry(effInd, entry.set, entry.predicate)
        }
      }

      val entries = assignIndices
      val subscript = entries.flatMap(_.indices)
      val s = entries.find(_.set == SExpr).toList.flatMap(_.indices).headOption | sIdeal // `sIdeal' shouldn't be needed here ...
      val t = entries.find(_.set == TExpr).toList.flatMap(_.indices).headOption | tIdeal // `tIdeal' shouldn't be needed here ...

      val s1 = dummy(maxStartingWith(subscript, s"${s.name}1", s.name) + "1")
      val s2 = dummy(maxStartingWith(subscript, s"${s.name}2", s.name) + "2")

      val subscript1 = subscript.map(x => if (x == s) s1 else x)
      val subscript2 = subscript.map(x => if (x == s) s2 else x)

      val indexing = IndExpr(entries, predicate)
      val x = func(xvar, indexing, s, t, S, link, s1, s2, subscript1, subscript2)
      x
    }

  }

  def nonanticipativityXFunc(
                              xvar: VarStat,
                              indexing: IndExpr,
                              s: DummyIndDecl,
                              t: DummyIndDecl,
                              S: SetStat,
                              link: ParamStat,
                              s1: DummyIndDecl,
                              s2: DummyIndDecl,
                              subscript1: List[DummyIndDecl],
                              subscript2: List[DummyIndDecl]): ConstraintStat = {

    val indexing1 = amphip.model.replace(indexing, s, s1)

    st(s"NonAnticipativity${ xvar.name }", indexing1) {
      sum(s2 in S)(link(s1, s2, t) * xvar(subscript2)) === sum(s2 in S)(link(s1, s2, t)) * xvar(subscript1)
    }
  }

  def nonanticipativityZFunc(
                              xvar: VarStat,
                              indexing: IndExpr,
                              s: DummyIndDecl,
                              t: DummyIndDecl,
                              S: SetStat,
                              link: ParamStat,
                              s1: DummyIndDecl,
                              s2: DummyIndDecl,
                              subscript1: List[DummyIndDecl],
                              subscript2: List[DummyIndDecl]): ConstraintStat = {

    import indexing._

    val SExpr = S()

    val indexing2 = IndExpr(entries.flatMap {
      case IndEntry(List(`s`), SExpr, _) => List(s1 in S, s2 in S)
      case x => List(x)
    }, predicate)

    st(s"NonAnticipativity${ xvar.name }", indexing2) {
      link(s1, s2, t) * xvar(subscript2) === link(s1, s2, t) * xvar(subscript1)
    }
  }

}