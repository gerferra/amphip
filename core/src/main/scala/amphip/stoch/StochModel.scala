package amphip.stoch

import scalaz.syntax.show._

import amphip.model.ast._
import amphip.model.dsl._
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

sealed trait NAMode
case class DenseNAMode(link: ParamStat, naForm: NAForm) extends NAMode
case class CompressedNAMode(link: SetStat) extends NAMode
case class AdaptedNAMode(T: SetStat) extends NAMode {
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

  val H = {
    val t = dummy("t")
    max(t in T)(t)
  }

  val ancf: ParamStat = {
    val t = dummy("t")
    val s = dummy("s")
    param("NA_ancf", s in ST(H), t in T) in ST(t) := anc(H, s, t)
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

    case AdaptedNAMode(innerT) => 
      require(innerT == T, s"Adapted NA mode stages set is different to model stages set (${innerT.shows} vs ${T.shows})")

  }
  
}
