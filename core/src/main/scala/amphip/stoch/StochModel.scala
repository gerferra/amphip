package amphip.stoch

import scalaz.syntax.show._
import cats.syntax.option._

import amphip.dsl._
import amphip.model.ast._
import amphip.model.dimen
import amphip.model.replace
import amphip.data._
import amphip.stoch.StochModel._

sealed trait StochModel {

  def model: ModelWithData
  def stochData: StochData
  def S: SetStat
  def p: ParamStat

}

case class TwoStageStochModel(model: ModelWithData, stochData: StochData, S: SetStat, p: ParamStat) extends StochModel {
  checkDomain(p.name, p.domain, List(S))
}

case class MultiStageStochModel(model: ModelWithData, stochData: StochData, T: SetStat, S: SetStat, p: ParamStat, naMode: NAMode) extends StochModel {
  checkDomain(p.name, p.domain, List(S))

  naMode match {
    case DenseNAMode(link, _) =>
      checkDomain(link.name, link.domain, List(S, S, T))

    case CompressedNAMode(link) =>
      checkDomain(link.name, link.domain, List(T))

    case STAdapter(innerT, innerS) => 
      require(innerT == T, s"STAdapter stages set is different to model stages set (${innerT.shows} vs ${T.shows})")
      require(innerS == S, s"STAdapter scenarios set is different to model scenarios set (${innerS.shows} vs ${S.shows})")

  }
  
}

object StochModel {
  def checkDomain(name: String, domain: Option[IndExpr], targetDomain: List[SetExpr]): Unit = 
    require({
      domain.fold(false)(_.entries.map(_.set) == targetDomain)
    }, s"`$name' must be indexed " + ind(targetDomain.map(x => x: IndEntry): _*).shows + " but was: " + domain.fold("not-indexed")(_.shows))

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
}

/* 
  TODO review non anticipativity schemas names. 
  - CompressedNAMode is likely to be removed
  - DenseNAMode ~> ExplicitNAMode?
  - AdaptedNAMode ~> ImplicitNAMode? 
    - This can be implicit or explicit, current implementation is explicit.
    - Maybe retain "adapted" and add parameter to specify if the non anticipative 
      version of variables and parameters should replace the originals 
      (this is implicit non anticipativity).
    - Using "STAdapter" for now
 */
sealed trait NAMode
case class DenseNAMode(link: ParamStat, naForm: NAForm) extends NAMode
case class CompressedNAMode(link: SetStat) extends NAMode
case class STAdapter(T: SetStat, S: SetStat) extends NAMode {
  val ST: SetStat = set("ST_ST", T)
  
  val pred: ParamStat = {
    val t = dummy("t")
    param("ST_pred", t in T &~ List(1), ST(t)) in ST(t-1)
  }
  
  lazy val anc: ParamStat = {
    val (t, tp) = (dummy("t"), dummy("tp"))
    val s = dummy("s")
    param("ST_anc", ind(t in T, s in ST(t), tp in T) | tp <= t) in ST(tp) :=
      xif (tp === t) { s } { anc(t-1, pred(t,s), tp) }
  }

  val H: ParamStat = {
    val t = dummy("t")
    param("ST_H") := max(t in T)(t)
  }

  val ancf: ParamStat = {
    val t = dummy("t")
    val s = dummy("s")
    param("ST_ancf", s in ST(H), t in T) in ST(t) := anc(H, s, t)
  }

  def adaptParam(param: ParamStat): Option[(ParamStat, ParamStat)] = {
    for {
      IndExpr(entries0, predicate0) <- param.domain
        if isStochastic(entries0, T, S)
    } yield {
      val tIdeal = dummy("t")
      val sIdeal = dummy("s")

      val (entries, t, s) = assignIndices(entries0, T, S, tIdeal, sIdeal)
      val param0          = param.copy(domain = IndExpr(entries, predicate0).some)
      
      /* 
      Adapted version of the parameter which don't have separated scenarios 
      and allows to specify the parameter values in a more compact way.
      */
      val ST_param = 
        replace(param0, S(), ST(t)).copy(name = s"ST_${param0.name}")
      
      val subscript  = entries.flatMap(_.indices)
      val subscript1 = subscript.map(x => if (x == s) ancf(s,t) else x: SimpleExpr)

      val paramA = param0 default ST_param(subscript1)

      paramA -> ST_param
    }
  }
}

sealed trait NAForm
object NAForm {
  case object X extends NAForm
  case object Z extends NAForm
}
