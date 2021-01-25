package amphip.stoch

import spire.math._
import spire.implicits._

import amphip.dsl._
import amphip.base._
import amphip.base.implicits._
import amphip.model.ast.ParamStat
import amphip.data.ModelData, ModelData.{SimpleNum, ParamStatData}
import amphip.stoch.StochData.Scenario

object EV {

  val singleBS = BasicScenario("EV_Single_BS")

  def apply(stochModel: StochModel): StochModel = {
    apply(stochModel, stochModel.stochData.stages.head)
  }

  def apply(stochModel: StochModel, stage: Stage): StochModel = {
    val stochData0  = stochModel.stochData
    val stages      = stochData0.stages
    val scenByStage = stochData0.byStage(stochData0.finalScenariosIdent)

    val stageIdx  = stages.indexOf(stage)
    val posStages =  
      for {
        (t,i) <- stages.zipWithIndex if i > stageIdx
      } yield {
        t
      }

    val posStagesSet   = posStages.toSet 
    val scenByStagePos = scenByStage.filter(p => posStagesSet(p._1))
    
    val stochDataEV = scenByStagePos.foldLeft(stochData0) {
      case (stochData1, (_, stageScenMap)) => 
        
        val stageScenariosP = 
          for {
            (historyP, bssP) <- stageScenMap.toList
            scenP            <- bssP.map(bsP => historyP :+ bsP)
          } yield {
            scenP
          }
        
        val stageGroups = 
          stageScenariosP.groupByLinked(_.unzip._1.take(stageIdx + 1))

        val stageSize = stageScenariosP.headOption.map(_.size).getOrElse(stageIdx + 1)

        stageGroups.foldLeft(stochData1) { 
          case (stochData2, (history, scenariosP)) => 

            val scenProb = 
              for {
                scenP         <- scenariosP 
                (scen, probs)  = scenP.unzip
              } yield {
                scen -> probs.qproduct
              }
            
            val scenData = 
              for {
                scenP <- scenariosP 
                scen   = scenP.unzip._1
              } yield {
                scen -> stochData0.paramData(scen)
              }

            val baseScen = history ::: List.fill(stageSize - stageIdx - 2)(singleBS)

            stochData2
              .customScenarios(baseScen, singleBS -> r"1")
              .scenarioData(baseScen :+ singleBS, evData(scenProb.toMap, scenData.toMap))
        }
    }
    
    stochModel match {
      case x: TwoStageStochModel   => x.copy(stochData = stochDataEV)
      case x: MultiStageStochModel => x.copy(stochData = stochDataEV)
    }
  }

  def evData(
    probs: Map[Scenario, Rational], 
    data : Map[Scenario, Map[ParamStat, ParamStatData]]): 
      Map[ParamStat, ParamStatData] = {
    
    val pairs =
      for {
        (scen, bsData)      <- data.toList
        (param, pData)      <- bsData
        (key, SimpleNum(v)) <- pData
        prob                <- probs.get(scen)
      } yield {
        (param -> key, prob * v)
      }

    val byParamKey = pairs.groupBy(_._1)

    val mean =
      for {
        ((param, key), group) <- byParamKey.toList
        nv                     = group.unzip._2.qsum
      } yield {
        (param, key -> SimpleNum(nv.toDouble))
      }

    val byParam =
      for {
        (param, group) <- mean.groupBy(_._1)
      } yield {
        param -> (LinkedMap() ++ group.unzip._2)
      }

    byParam.toMap
  }
}
