package amphip.stoch

import cats.syntax.option._

import spire.math
import spire.implicits._

import amphip.dsl._
import amphip.model.ast._
import amphip.stoch.StochModel.{isStochastic, assignIndices}

object nonanticipativity {

  def apply(xvar: VarStat, T: SetStat, S: SetStat, na: STAdapter): Option[ConstraintStat] = {
    for {
      IndExpr(entries0, predicate0) <- xvar.domain 
        if isStochastic(entries0, T, S)
    } yield {
      val tIdeal = dummy("t")
      val sIdeal = dummy("s")

      val (entries, t, s) = assignIndices(entries0, T, S, tIdeal, sIdeal)
      val subscript       = entries.flatMap(_.indices)
      
      val s1 = uniqueDummy(subscript, s, 1)
      val s2 = uniqueDummy(subscript, s, 2)
      val ancfEq = na.ancf(s1, t) === na.ancf(s2, t)
      
      val detEntries = entries.filterNot(e => List(T(), S()).contains(e.set))
      
      val indexing = IndExpr(
          (t in T) :: (s1 in S) :: (s2 in S) :: detEntries, 
          predicate0.fold(ancfEq)(ancfEq && _).some)

      val subscript1 = subscript.map(x => if (x == s) s1 else x)
      val subscript2 = subscript.map(x => if (x == s) s2 else x)

      st(s"NA_${xvar.name}_ctr", indexing) {
        xvar(subscript1) === xvar(subscript2)
      }
    }
  }

  def uniqueDummy(subscript: List[DummyIndDecl], base: DummyIndDecl, target: Int): DummyIndDecl = {
    val name = s"${maxStartingWith(subscript, base.name + target, base.name)}$target"
    DummyIndDecl(name, synthetic = true)
  }

  /* juggling to avoid name colisions */
  def maxStartingWith(subscript: List[DummyIndDecl], start: String, default: String): String = {
    subscript.filter(_.name.startsWith(start)).map(_.name).fold(default)(math.max[String])
  }

  // old implementations

  def apply(xvar: VarStat, T: SetStat, S: SetStat, link: SetStat): Option[ConstraintStat] = {

    for {
      IndExpr(entries0, predicate) <- xvar.domain
        if isStochastic(entries0, T, S)
    } yield {

      val TExpr = T()
      val SExpr = S()

      val tIdeal = dummy("t")
      val sIdeal = dummy("s")

      val (entries, t, s) = assignIndices(entries0, T, S, tIdeal, sIdeal)
      val subscript = entries.flatMap(_.indices)

      val s1 = dummy(maxStartingWith(subscript, s"${s.name}1", s.name) + "1")
      val s2 = dummy(maxStartingWith(subscript, s"${s.name}2", s.name) + "2")

      val subscript1 = subscript.map(x => if (x == s) (s + 1) else (x: NumExpr))

      val naPred = s >= s1 && (s + 1) <= s2

      val indexing = IndExpr(
          (t in TExpr) :: 
          ((s1, s2) in link(t)) :: 
          (s in SExpr) :: 
          entries.filterNot(e => List(SExpr, TExpr).contains(e.set)), 
          predicate.fold(naPred)(naPred && _).some)

      st(s"NA_${xvar.name}", indexing) {
        xvar(subscript) === xvar(subscript1)
      }
    }
  }

  def nonanticipativityTemplate(xvar: VarStat, T: SetStat, S: SetStat, link: ParamStat,
                                func: (VarStat, IndExpr, DummyIndDecl, DummyIndDecl, SetStat, ParamStat, DummyIndDecl, DummyIndDecl, List[DummyIndDecl], List[DummyIndDecl]) => ConstraintStat): Option[ConstraintStat] = {

    for {
      IndExpr(entries0, predicate) <- xvar.domain
        if isStochastic(entries0, T, S)
    } yield {

      val sIdeal = dummy("s")
      val tIdeal = dummy("t")

      val (entries, t, s) = assignIndices(entries0, T, S, tIdeal, sIdeal)
      val subscript = entries.flatMap(_.indices)

      val s1 = uniqueDummy(subscript, s, 1)
      val s2 = uniqueDummy(subscript, s, 2)

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