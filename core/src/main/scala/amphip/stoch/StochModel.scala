package amphip.stoch

import scalaz.syntax.show._

import amphip.model.ast._
import amphip.model.dsl._
import amphip.data._

sealed trait StochModel {

  def model: ModelWithData
  def stochData: StochData
  def S: SetStat
  def p: ParamStat

}

sealed trait NAMode
case class DenseNAMode(link: ParamStat, naForm: NAForm) extends NAMode
case class CompressedNAMode(link: SetStat) extends NAMode

sealed trait NAForm
object NAForm {
  case object X extends NAForm
  case object Z extends NAForm
}

case class TwoStageStochModel(model: ModelWithData, stochData: StochData, S: SetStat, p: ParamStat) extends StochModel {
  require({
    p.domain.fold(false)(_.entries.map(_.set) == List(S()))
  }, "`p' must be indexed " + ind(S).shows + " but was: " + p.domain.fold("not-indexed")(_.shows))
}

case class MultiStageStochModel(model: ModelWithData, stochData: StochData, S: SetStat, T: SetStat, p: ParamStat, naMode: NAMode) extends StochModel {
  require({
    p.domain.fold(false)(_.entries.map(_.set) == List(S()))
  }, "`p' must be indexed " + ind(S).shows + " but was: " + p.domain.fold("not-indexed")(_.shows))

  naMode match {
    case DenseNAMode(link, _) =>
      require({
        link.domain.fold(false)(_.entries.map(_.set) == List(S(), S(), T()))
      }, "`link' must be indexed " + ind(S, S, T).shows + "  but was: " + link.domain.fold("not-indexed")(_.shows))     

    case CompressedNAMode(link) =>
      require({
        link.domain.fold(false)(_.entries.map(_.set) == List(T()))
      }, "`link' must be indexed " + ind(T).shows + "  but was: " + link.domain.fold("not-indexed")(_.shows))     
  }
  
}
