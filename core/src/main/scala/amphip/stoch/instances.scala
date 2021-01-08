package amphip.stoch

import amphip.data.dsl._
import amphip.model.replace, replace.{rep_, Replace}

object instances extends AllInstances

trait AllInstances {

  implicit val StochModelReplace: Replace[StochModel] = new Replace[StochModel] {
    def rep[B](x: StochModel, pv: B, nv: B): StochModel = rep_(x, pv, nv) {
      case TwoStageStochModel(model, stochData, S @ _, pi) =>
        TwoStageStochModel(
          model.replace(pv, nv), 
          stochData, 
          replace(S , pv, nv), 
          replace(pi, pv, nv))

      case MultiStageStochModel(model, stochData, T @ _, S @ _, pi, naMode) =>
        val newNAMode = naMode match {
          case CompressedNAMode(link)    => CompressedNAMode(replace(link, pv, nv))
          case DenseNAMode(link, naForm) => DenseNAMode     (replace(link, pv, nv), naForm)
          case STAdapter(T @ _, S @ _)   => STAdapter(replace(T, pv, nv), replace(S, pv, nv))
        }
        MultiStageStochModel(
          model.replace(pv, nv), 
          stochData, 
          replace(T , pv, nv), 
          replace(S , pv, nv), 
          replace(pi, pv, nv), 
          newNAMode)
    }
  }

}