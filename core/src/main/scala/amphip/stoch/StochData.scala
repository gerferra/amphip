package amphip.stoch

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

import scalaz.std.option.optionSyntax._
import scalaz.syntax.show._
import scalaz.syntax.std.boolean._
import scalaz.Scalaz.stringInstance
//import scalaz.std.list.listSyntax._
import cats.syntax.list._


import spire.math._
import spire.implicits._

import amphip.base._
import amphip.base.implicits._
import amphip.model.ast._
import amphip.model.dsl._
import amphip.data.dsl._
import amphip.data._
import amphip.data.ModelData._

import StochData._

case class StochData private (
    stages: List[Stage],
    basicScenarios: LinkedMap[Stage, LinkedMap[BasicScenario, Rational]],
    customScenarios: LinkedMap[Scenario, LinkedMap[BasicScenario, Rational]],
    deletedScenarios: LinkedMap[Scenario, Set[BasicScenario]],
    defaults: LinkedMap[ParamStat, ParamStatData],
    basicData: LinkedMap[Stage, LinkedMap[BasicScenario, LinkedMap[ParamStat, ParamStatData]]],
    scenarioData: LinkedMap[Scenario, LinkedMap[ParamStat, ParamStatData]],
    separated: Boolean) {

  def stages(ts: Stage*): StochData = copy(stages = ts.toList)

  /* always normalizes probabilities to 1 */
  def basicScenarios(t: Stage, bss: (BasicScenario, Rational)*): StochData = {
    val bssMap = LinkedMap(bss: _*)
    val totProb = bssMap.unzip._2.qsum
    require(totProb != 0, "sum of probabilities equals zero")

    copy(basicScenarios = basicScenarios + (t -> normalize(bssMap)))
  }

  /* 
    Replaces the `BasicScenario`s at `history` with the specified `replacement`.
    The basic scenarios not included in the replacement list are marked as deleted.
    The probabilities are always normalized to 1.
  */
  def customScenarios(history: Scenario, replacement: (BasicScenario, Rational)*): StochData = {
    val replacementMap = LinkedMap(replacement: _*)
    require(replacementMap.unzip._2.qsum != 0, "sum of probabilities equals zero")

    val deletedBS = 
      (for {
        t        <- stages.lift(history.size)
        bss      <- basicScenarios.get(t)
        cbssBS    = replacementMap.unzip._1.toList
        dbss      = bss.unzip._1.filter { !cbssBS.contains(_) }.toSet
      } yield {
        dbss
      }) | Set.empty

    val currDS = deletedScenarios.getOrElse(history, Set.empty)
    val normalized = normalize(replacementMap) 

    copy(
      customScenarios = customScenarios + (history -> normalized),
      deletedScenarios = deletedScenarios + (history -> (currDS ++ deletedBS)))
  }

  def default(p: ParamStat, values: ParamStatData): StochData = {
    copy(defaults = defaults + (p -> values))
  }

  def basicData(t: Stage, bs: BasicScenario, p: ParamStat, values: ParamStatData): StochData = {
    val valuesByStage = basicData.getOrElse(t, LinkedMap.empty)
    val valuesByBS = valuesByStage.getOrElse(bs, LinkedMap.empty)

    val newValuesByBS = valuesByBS + (p -> values)
    val newValuesByStage = valuesByStage + (bs -> newValuesByBS)
    val newBasicData = basicData + (t -> newValuesByStage)

    copy(basicData = newBasicData)
  }

  def scenarioData(scen: Scenario, p: ParamStat, values: ParamStatData): StochData = {
    val valuesByScen = scenarioData.getOrElse(scen, LinkedMap.empty)
    val newValuesByScen = valuesByScen + (p -> values)
    val newScenarioData = scenarioData + (scen -> newValuesByScen)

    copy(scenarioData = newScenarioData)
  }

  def separated(x: Boolean): StochData = copy(separated = x)

  lazy private[this] val balancedTreeIdent: List[List[(BasicScenario, Rational)]] = balancedTree(List.empty, identity)

  lazy val balancedTree: List[Scenario] = balancedTreeIdent.map(_.map(_._1)) //balancedTree(List.empty, _._1)

  lazy val deletedScenariosList: List[Scenario] = 
    for {
      (history, bss) <- deletedScenarios.toList
      deleted        <- bss.map(bs => history :+ bs)
    } yield {
      deleted
    }

  def balancedTree[T](seed: List[T], ext: ((BasicScenario, Rational)) => T): List[List[T]] = {
    val rstages = stages.drop(seed.size)
    val basis = basicScenarios.filterKeys(rstages.contains).mapValues(_.map(ext).toList)

    val zero = seed.toNel.cata(x => List(x.toList.reverse), Nil)

    val tree = basis.keys.foldLeft(zero) { (data, t) =>
      val bss = basis(t)
      if (data.isEmpty)
        bss.map(List(_))
      else
        for {
          s <- data
          bs <- bss
        } yield {
          bs :: s
        }
    }
    tree.map(_.reverse)
  }

  lazy val finalScenarios: List[Scenario] = {
    val customTree =
      for {
        (history, bss) <- customScenarios.toList
        scen           <- bss.map(p => history :+ p._1)
        scenTree       <- balancedTree(scen, _._1) 
          // avoids generating scenarios shorter than `stages.size` if there are 
          // no basic scenarios, ie, `balancedTree` generates nothing.
          if scenTree.size == stages.size 
      } yield {
        scenTree
      }

    val base = (customTree ::: balancedTree).distinct
      
    val target = base.filter { ss => 
      deletedScenariosList.forall(!ss.startsWith(_))
    }

    // TODO check why scala is faster than using spire sorting ...
    
    import Ordering.Implicits._
    target.sortBy(bsIndex)
  }

  def byStage[T](scenarios: List[List[T]]): LinkedMap[Stage, LinkedMap[List[T], List[T]]] = {
    LinkedMap() ++ stages.map(x => x -> onStage(scenarios, x))
  }

  def onStage[T](scenarios: List[List[T]], t: Stage): LinkedMap[List[T], List[T]] = {
    val ind = stages.indexOf(t)
    val group = scenarios.groupByLinked(_.take(ind))
    group.map { case (k, v) => k -> v.map(x => x(ind)).distinct }
  }

  lazy val scenariosByStage: LinkedMap[Stage, List[Scenario]] =
    for {
      (stage, values) <- byStage(finalScenarios)
      scenarios = values.flatMap { case (history, bss) => bss.map(history :+ _) }.toList
    } yield {
      stage -> scenarios
    }

  lazy val scenarios: List[Scenario] = scenariosByStage.values.flatten.toList

  lazy val numScenarios: Int = finalScenarios.size
  lazy val numStages   : Int = stages.size

  def TData: Range = 1 to numStages
  def SData: Range = 1 to numScenarios

  def STData: List[(Int, Range)] = {
    for {
      (stage, t_) <- stages.zipWithIndex
      sts         <- scenariosByStage.get(stage)
    } yield {
      (t_ + 1) -> (1 to sts.size)
    }
  }

  lazy val predecessorsData: List[(List[Int], Int)] = {
    for {
      (stage, t_) <- stages.zipWithIndex
      sts         <- scenariosByStage.get(stage).toList
      (scen, s_)  <- sts.zipWithIndex
      t            = t_ + 1
      s            = s_ + 1
      sPred       <- predecesor(t, s)
    } yield {
      List(t, s) -> sPred
    }
  }

  def predecesor(t: Int, s: Int): Option[Int] = 
    for {
      stage       <- stages.lift(t - 1)
      sts         <- scenariosByStage.get(stage)
      (scen, _)   <- sts.zipWithIndex
                      .find { 
                        case (_, s_) => s_ == s - 1
                      }
      pred         = scen.take(t - 1)
      stagePred   <- stages.lift(t - 2) 
      stsPred     <- scenariosByStage.get(stagePred)
      (_, sPred_) <- stsPred.zipWithIndex
                      .find { 
                        case (scen, _) => scen == pred
                      }
    } yield {
      sPred_ + 1
    }

  lazy val finalProbabilities: List[List[Rational]] = {

    /* probabilities of basic scenarios tree taking into account deleted scenarios */
    val balancedProbs        = balancedTreeIdent
    val balancedProbsByStage =
      for {
        (stage, bTree) <- byStage(balancedProbs)
      } yield {
        stage ->
          (for {
            (historyP, bss) <- bTree
            history          = historyP.unzip._1
            usedBS           = 
              bss.filter { case (bs, _) =>
                val ss = history :+ bs
                deletedScenariosList.forall(!ss.startsWith(_))
              }
            if usedBS.nonEmpty
          } yield {
            history -> (LinkedMap() ++ usedBS)
          }).toMap
      }

    val probs =
      for {
        scen <- finalScenarios
      } yield {
        for {
          p <- scen.zipWithIndex
          history = scen.take(p._2)
          customProb = 
            for {
              cbss  <- customScenarios.get(history)
              cprob <- cbss.get(p._1)
            } yield {
              cprob
            }
          basicProb = 
            for {
              t     <- stages.lift(p._2)
              bTree <- balancedProbsByStage.get(t)
              bss   <- bTree.get(history)
              prob  <- bss.get(p._1)
            } yield {
              prob
            }
          prob <- customProb.orElse(basicProb)
        } yield {
          prob
        }
      }

    probs
  }

  def probabilityData: List[Double] = {
    for {
      path <- finalProbabilities
    } yield {
      path.map(_.toDouble).product
    }
  }

  def probabilityData2: List[Double] = {
    for {
      path <- finalProbabilities
    } yield {
      //path.map(_.toDouble).product
      path.qproduct.toDouble
    }
  }

  /**
    Exact version of `probabilityData`. Currently is not used.
   */
  def probabilityDataExact: List[Rational] = {
    for {
      path <- finalProbabilities
    } yield {
      path.qproduct
    }
  }


  // Non-anticipativity handling

  /*
    Scenarios (including non-final) corresponding to each pair of final 
    scenario index and stage index, with sparated scenarios.
   */
  lazy private[stoch] val prefix: Array[Array[Scenario]] = {
    val res = Array.ofDim[Scenario](numScenarios, numStages)
    for {
      (scen, s) <- finalScenarios.zipWithIndex
    } {
      cfor(0)(_ < numStages, _ + 1) { t =>
        res(s)(t) = scen.take(t + 1)
      }
    }
    res
  }

  def initPrefixes(): Unit = {prefix; ()}

  private[this] val offset = separated.fold(1, 0)

  def samePrefix(s1: Int, s2: Int, t: Int): Boolean = {
    val t_ = t - 1 - offset
    if (t_ == -1) {
      /* if it's a separeted model (offset == 1), on the first stage 
         all the scenarios are the same */
      true
    } else {
      val s1_ = s1 - 1
      val s2_ = s2 - 1
      prefix(s1_)(t_) == prefix(s2_)(t_)
    }
  }

  lazy val linkDataFull: List[(List[Int], Int)] = {
    (for {
      s1 <- SData
      s2 <- SData
      t <- TData
    } yield {
      List(s1, s2, t) -> samePrefix(s1, s2, t).fold(1, 0)
    }).toList
  }

  lazy val linkDataFullDiagonal: List[(List[Int], Int)] = {
    (for {
      s1 <- SData
      s2 <- s1 to numScenarios
      t <- TData
      same = samePrefix(s1, s2, t)
      value = same.fold(1, 0)
      data <- if (s1 == s2) List(List(s1, s2, t) -> value) 
              else List(List(s1, s2, t) -> value, List(s2, s1, t) -> value)
    } yield {
      data
    }).toList
  }

  lazy val linkDataWithDefault: List[(List[Int], Int)] = {
    (for {
      s1 <- SData
      s2 <- SData
      t <- TData
      if samePrefix(s1, s2, t)
    } yield {
      List(s1, s2, t) -> 1
    }).toList
  }
   
  lazy val linkDataWithDefaultDiagonal: List[(List[Int], Int)] = {
    (for {
      s1 <- SData
      s2 <- s1 to numScenarios
      t <- TData
      if samePrefix(s1, s2, t)
      data <- if (s1 == s2) List(List(s1, s2, t) -> 1) 
              else List(List(s1, s2, t) -> 1, List(s2, s1, t) -> 1)
    } yield {
      data
    }).toList
  }

  def linkData: List[(List[Int], Int)] = linkDataFull

  /* 
    BasicScenario corresponding to each combination of final scenario index
    and stage index, with sparated scenarios.
   */
  lazy private[stoch] val matrix: Array[Array[BasicScenario]] = {
    val res = Array.ofDim[BasicScenario](numScenarios, numStages)
    for {
      (scen, s) <- finalScenarios.zipWithIndex
      (bs, t) <- scen.zipWithIndex
    } {
      res(s)(t) = bs
    }
    res
  }

  def initMatrix(): Unit = {matrix; ()}


  // TODO take into account already separated models
  def linkSetDataBounds: List[(Int, List[(Int, Int)])] = 
    linkSetDataBounds(false)

  def linkSetDataBounds(includeSingleRanges: Boolean): List[(Int, List[(Int, Int)])] = {
    val minDiff = includeSingleRanges.fold(0, 1)
    val res = ListBuffer.empty[(Int, List[(Int, Int)])]
    if (numScenarios > 0) {
      cfor(0)(_ < numStages, _ + 1) { t_ =>
        val t = t_ + 1
        val stageData = ListBuffer.empty[(Int, Int)]
        //var lastBS: BasicScenario = matrix(0)(t_)
        var ini = 1
        var end = 1
        cfor(1)(_ < numScenarios, _ + 1) { s_ =>
          val s = s_ + 1
          if (samePrefix(ini, s, t)) {
            end = s
          } else {
            if (end - ini >= minDiff) stageData += ini -> end
            ini = s
            end = s
          }
          /*if (matrix(s_)(t_) == lastBS) {
            end = s
          } else {
            if (end - ini >= minDiff) stageData += ini -> end
            ini = s
            end = s
          }
          lastBS = matrix(s_)(t_)
          */
        }
        if (end - ini >= minDiff) stageData += ini -> end
        res += t -> stageData.toList
      }
    }
    res.toList
  }


  // Parameters

  def parameters: List[ParamStat] = {
    val defP = defaults.keys

    val basicP =
      for {
        v1 <- basicData.values
        v2 <- v1.values
        p <- v2.keys
      } yield p

    val scenP =
      for {
        v <- scenarioData.values
        p <- v.keys
      } yield p

    (defP ++ basicP ++ scenP).toList.distinct
  }

  def parametersData: List[(ParamStat, List[(List[SimpleData], SimpleData)])] =
    parameters.map(p => p -> paramData(p))

  def paramData(param: ParamStat): List[(List[SimpleData], SimpleData)] = {
    val scenariosData =
      for {
        s <- SData
        t <- TData
        s_ = s - 1
        t_ = t - 1
        scenPrefix = prefix(s_)(t_)
        bs = matrix(s_)(t_)
        cData = for {
          cbss <- scenarioData.get(scenPrefix)
          data <- cbss.get(param)
        } yield {
          data
        }
        bData = for {
          stage <- stages.lift(t_)
          bss <- basicData.get(stage)
          ds <- bss.get(bs)
          data <- ds.get(param)
        } yield {
          data
        }
        defaultData = defaults.get(param)
        pData <- cData.orElse(bData).orElse(defaultData).toList
        (key, value) <- pData
      } yield {
        (List[SimpleData](t, s) ::: key.subscript) -> value
      }

    scenariosData.toList
  }


  def paramDataST(param: ParamStat): List[(List[SimpleData], SimpleData)] = {
    val scenariosData =
      for {
        (stage, t_) <- stages.zipWithIndex
        sts         <- scenariosByStage.get(stage).toList
        (scen, s_)  <- sts.zipWithIndex
        cData = 
          for {
            cbss <- scenarioData.get(scen)
            data <- cbss.get(param)
          } yield {
            data
          }
        bs          <- scen.lastOption.toList
        bData = 
          for {
            bss  <- basicData.get(stage)
            ds   <- bss.get(bs)
            data <- ds.get(param)
          } yield {
            data
          }
        defaultData   = defaults.get(param)
        pData        <- cData.orElse(bData).orElse(defaultData).toList
        t             = t_ + 1
        s             = s_ + 1
        (key, value) <- pData
      } yield {
        (List[SimpleData](t, s) ::: key.subscript) -> value
      }

    scenariosData.toList
  }

  def parametersDataBounds: List[(ParamStat, List[(List[SimpleData], SimpleData)])] =
    parameters.map(p => p -> paramDataBounds(p))

  def paramDataBounds(param: ParamStat): List[(List[SimpleData], SimpleData)] = {
    val scenariosData =
      for {
        (t, tup) <- linkSetDataBounds(includeSingleRanges = true)
        (s1, s2) <- tup
        t_ = t - 1
        s_ = s1 - 1
        scenPrefix = prefix(s_)(t_)
        bs = matrix(s_)(t_)
        cData = for {
          cbss <- scenarioData.get(scenPrefix)
          data <- cbss.get(param)
        } yield {
          data
        }
        bData = for {
          stage <- stages.lift(t_)
          bss <- basicData.get(stage)
          ds <- bss.get(bs)
          data <- ds.get(param)
        } yield {
          data
        }
        defaultData = defaults.get(param)
        pData <- cData.orElse(bData).orElse(defaultData).toList
        (key, value) <- pData
      } yield {
        (List[SimpleData](t, s1, s2) ::: key.subscript) -> value
      }

    scenariosData 
  }

  //// AUX

  private[this] def normalize(map: LinkedMap[BasicScenario, Rational], target: Rational = 1): LinkedMap[BasicScenario, Rational] = {
    val sum = map.unzip._2.qsum
    require(sum != 0 || target == 0, "sum of probabilities equals zero")
    val coef = if (target == 0) Rational.zero else sum / target
    val updFunc: Rational => Rational = if (coef == 0) { _ => Rational.zero } else { _ / coef }
    map.map { case (k, v) => k -> updFunc(v) }
  }

  private[this] def bsIndex(scen: Scenario): List[Int] = {
    scen.zipWithIndex.map {
      case (bs, i) => basicScenarios.toList.lift(i).cata(x =>
        {
          val ind = x._2.unzip._1.toList.indexOf(bs)
          if (ind == -1) Int.MaxValue else ind
        },
        Int.MaxValue)
    }
  }

}

object StochData {
  // XXX should be `amphip.stoch.Scenario`
  type Scenario = List[BasicScenario]

  def apply(): StochData = new StochData(
    stages = Nil, 
    basicScenarios = LinkedMap.empty, 
    customScenarios = LinkedMap.empty, 
    deletedScenarios = LinkedMap.empty, 
    defaults = LinkedMap.empty, 
    basicData = LinkedMap.empty, 
    scenarioData = LinkedMap.empty, 
    separated = false)

  def filter(modelData: ModelData, p: ParamStat): ModelData =
    modelData.filterParams(_.name == p.name)

  def filter(modelData: ModelData, s: SetStat): ModelData =
    modelData.filterSets(_.name == s.name)

  def requireStochastic(p: ParamStat, model: StochModel): Unit = {
    require(isStochastic(p, model), {
      val stochIndExpr = IndExpr(stochIndices(model))
      val pIndex = p.domain.cata(_.shows, "not-indexed")
      s"Index of parameter `${p.name}' must start with ${stochIndExpr.shows} (index: $pIndex)"
    })
  }

  // stricter requirement for stochastic parameters
  def isStochastic(p: ParamStat, model: StochModel): Boolean = p match {
    case ParamStat(_, _, Some(indexing), _) =>
      val sind = stochIndices(model)
      val psind = indexing.entries.take(sind.size).map(_.set: IndEntry)
      psind == sind
    case _ => false
  }

  def stochIndices(model: StochModel): List[IndEntry] =
    model match {
      case m: TwoStageStochModel => List(m.S())
      case m: MultiStageStochModel => List(m.T(), m.S())
    }

  def asDet(p: ParamStat, model: StochModel): ParamStat = p match {
    case ParamStat(_, _, Some(indexing), _) =>
      val newEntries = indexing.entries.drop(stochIndices(model).size)
      p.copy(domain = indexing.copy(entries = newEntries).some)
    case _ => p
  }
}

case class Stage(name: String /*, duration? */ )
object Stage {
  implicit def StringToStage(str: String): Stage = Stage(str)
}
case class BasicScenario(name: String) {
  override def toString = name.shows
}
object BasicScenario {
  implicit def StringToBasicScenario(str: String): BasicScenario = BasicScenario(str)
}
