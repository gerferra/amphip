package amphip.stoch

import scalaz.syntax.show._
import cats.syntax.option._

import amphip.model.ast._
import amphip.model.dsl._
import amphip.model.replace
import amphip.data._
import amphip.stoch.StochModel._

sealed trait StochModel {

  def model: ModelWithData
  def stochData: StochData
  def S: SetStat
  def p: ParamStat

}
object StochModel {
  def checkDomain(name: String, domain: Option[IndExpr], targetDomain: List[SetExpr]): Unit = 
    require({
      domain.fold(false)(_.entries.map(_.set) == targetDomain)
    }, s"`$name' must be indexed " + ind(targetDomain.map(x => x: IndEntry): _*).shows + " but was: " + domain.fold("not-indexed")(_.shows))

}

/* 
  TODO review non anticipativity schemas names. 
  - CompressedNAMode is likely to be removed
  - DenseNAMode ~> ExplicitNAMode?
  - AdaptedNAMode ~> ImplicitNAMode?
 */
sealed trait NAMode
case class DenseNAMode(link: ParamStat, naForm: NAForm) extends NAMode
case class CompressedNAMode(link: SetStat) extends NAMode
case class AdaptedNAMode(T: SetStat, S: SetStat) extends NAMode {
  val ST: SetStat = set("NA_ST", T)
  
  val pred: ParamStat = {
    val t = dummy("t")
    param("NA_pred", t in T &~ List(1), ST(t)) in ST(t-1)
  }
  
  lazy val anc: ParamStat = {
    val (t, tp) = (dummy("t"), dummy("tp"))
    val s = dummy("s")
    param("NA_anc", ind(t in T, s in ST(t), tp in T) | tp <= t) in ST(tp) :=
      cond (tp === t) { s } { anc(t-1, pred(t,s), tp) }
  }

  val H: ParamStat = {
    val t = dummy("t")
    param("NA_H") := max(t in T)(t)
  }

  val ancf: ParamStat = {
    val t = dummy("t")
    val s = dummy("s")
    param("NA_ancf", s in ST(H), t in T) in ST(t) := anc(H, s, t)
  }

  def adaptParam(param: ParamStat): Option[(ParamStat, ParamStat)] =
    for {
      IndExpr(entries0, predicate) <- param.domain
    } yield {
      val TExpr = T()
      val SExpr = S()

      val tIdeal = dummy("t")
      val sIdeal = dummy("s")

      val entries = nonanticipativity.assignIndices(entries0, T, S, sIdeal, tIdeal)
      val subscript = entries.flatMap(_.indices)
      val t = entries.find(_.set == TExpr).toList.flatMap(_.indices).headOption getOrElse tIdeal 
      val s = entries.find(_.set == SExpr).toList.flatMap(_.indices).headOption getOrElse sIdeal 

      val param0 = param.copy(domain = IndExpr(entries, predicate).some)

      /* 
        Adapted version of the parameter which don't have separated scenarios 
        and allows to specify the parameter values in a more compact way.
       */
      val NA_param = 
        replace(param0, SExpr, ST(t))
        .copy(name = s"NA_${param0.name}")

      val subscript1: List[SimpleExpr] = subscript.map(x => if (x == s) ancf(s,t) else x: SimpleExpr)

      val param_ = param0 default NA_param(subscript1)

      param_ -> NA_param
    }
}

sealed trait NAForm
object NAForm {
  case object X extends NAForm
  case object Z extends NAForm
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

    case AdaptedNAMode(innerT, innerS) => 
      require(innerT == T, s"Adapted NA mode stages set is different to model stages set (${innerT.shows} vs ${T.shows})")
      require(innerS == S, s"Adapted NA mode scenarios set is different to model scenarios set (${innerS.shows} vs ${S.shows})")

  }
  
}
