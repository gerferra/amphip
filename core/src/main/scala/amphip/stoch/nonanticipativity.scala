package amphip.stoch

import cats.syntax.option._

import spire.math
import spire.implicits._

import amphip.dsl._
import amphip.model.ast._
import amphip.model.dimen

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

  def isStochastic(entries: List[IndEntry], T: SetStat, S: SetStat): Boolean = {
    val dependsOnT = entries.exists(_.set == T())
    val dependsOnS = entries.exists(_.set == S())
    dependsOnT && dependsOnS
  }

  /* Calculate the indices needed for each set expression in the entries 
   * and return assigned indices to T and S sets.
   */
  def assignIndices(entries0: List[IndEntry], T: SetStat, S: SetStat, tIdeal: DummyIndDecl, sIdeal: DummyIndDecl): (List[IndEntry], DummyIndDecl, DummyIndDecl) = {
    val gen = newGen

    // to avoid collisions
    val indices = entries0.flatMap(_.indices)
    val tIdealA = indices.find(_ == tIdeal).fold(tIdeal) { _ =>
      DummyIndDecl(gen.dummy(tIdeal.name).freshName, synthetic = true)
    }
    val sIdealA = indices.find(_ == sIdeal).fold(sIdeal) { _ =>
      DummyIndDecl(gen.dummy(sIdeal.name).freshName, synthetic = true)
    }

    val TExpr = T()
    val SExpr = S()

    val (revEntries, tA, sA) = 
      entries0.foldLeft((List.empty[IndEntry], tIdealA, sIdealA)) { case ((res, t, s), entry) => 
        val indices = entry.indices

        val ind0 =
          if (indices.isEmpty) {
            val dim = dimen(entry.set)
            List.fill(dim)(DummyIndDecl(gen.dummy(nameHint(entry.set)).freshName, synthetic = true))
          } else {
            indices
          }

        // uses tIdealA and sIdealA if possible
        val (indA, tA, sA) = 
          (entry.set, ind0) match {
            // index was generated, replace
            case (TExpr, List(t0)) if t0.synthetic  => (List(t), t , s)
            case (SExpr, List(s0)) if s0.synthetic  => (List(s), t , s)
            // index was not generted, update tIdealA and sIdealA
            case (TExpr, List(t0)) if !t0.synthetic => (ind0   , t0, s)
            case (SExpr, List(s0)) if !s0.synthetic => (ind0   , t , s0)
            // not T or S
            case _                                  => (ind0   , t , s)
          }

        (IndEntry(indA, entry.set, entry.predicate) :: res, tA, sA)
      }

    (revEntries.reverse, tA, sA)
  }

  /* hint to use as name of the dummy index */
  def nameHint(expr: SetExpr): Option[SymName] = {
    def isValidChar(c: Char) = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_'
    import scalaz.syntax.show._
    val str = expr.shows
    str.filter(isValidChar).take(1).toLowerCase.some
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
      val s = entries.find(_.set == SExpr).toList.flatMap(_.indices).headOption getOrElse sIdeal // `sIdeal' shouldn't be needed here ...
      val t = entries.find(_.set == TExpr).toList.flatMap(_.indices).headOption getOrElse tIdeal // `tIdeal' shouldn't be needed here ...

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