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

// TODO handle deleted scenarios ...
case class StochData private (
    stages: List[Stage],
    basicScenarios: LinkedMap[Stage, LinkedMap[BasicScenario, Rational]],
    customScenarios: LinkedMap[Scenario, LinkedMap[BasicScenario, Rational]],
    defaults: LinkedMap[ParamStat, ParamStatData],
    basicData: LinkedMap[Stage, LinkedMap[BasicScenario, LinkedMap[ParamStat, ParamStatData]]],
    scenarioData: LinkedMap[Scenario, LinkedMap[ParamStat, ParamStatData]],
    separated: Boolean) {

  def stages(ts: Stage*): StochData = copy(stages = ts.toList)

  /* always normalize probabilities to 1 */
  def basicScenarios(t: Stage, bss: (BasicScenario, Rational)*): StochData = {
    val bssMap = LinkedMap(bss: _*)
    val totProb = bssMap.unzip._2.qsum
    require(totProb != 0, "sum of probabilities equals zero")

    copy(basicScenarios = basicScenarios + (t -> normalize(bssMap)))
  }

  /* probabilities are normalized later (in general) */
  def customScenarios(history: Scenario, changes: (BasicScenario, Rational)*): StochData = {
    val changesMap = LinkedMap(changes: _*)
    require(changesMap.unzip._2.qsum <= 1, "sum of probabilities above one")

    val checked =
      for {
        t        <- stages.lift(history.size)
        bss      <- basicScenarios.get(t)
        cbssBS    = changesMap.unzip._1.toList
        cbssProb  = changesMap.unzip._2
        rbss      = bss.filter { case (k, _) => !cbssBS.contains(k) }
        csum      = cbssProb.qsum
        rsum      = rbss.unzip._2.qsum
      } yield {
        /* 
          if the custom scenarios will replace all the basic scenarios
          (`rsum == 0`) the they must be normalized now
        */
        if (rsum == 0 && csum != 1) {
          normalize(changesMap)
        } else {
          changesMap
        }
      }

    val normalized = checked | normalize(changesMap) // the only data for these scenarios will come from custom scenarios -> must sum 1

    copy(customScenarios = customScenarios + (history -> normalized))
  }

  def default[B](p: ParamStat, values: ParamStatData): StochData = {
    copy(defaults = defaults + (p -> values))
  }

  def basicData[B](t: Stage, bs: BasicScenario, p: ParamStat, values: ParamStatData): StochData = {
    val valuesByStage = basicData.getOrElse(t, LinkedMap.empty)
    val valuesByBS = valuesByStage.getOrElse(bs, LinkedMap.empty)

    val newValuesByBS = valuesByBS + (p -> values)
    val newValuesByStage = valuesByStage + (bs -> newValuesByBS)
    val newBasicData = basicData + (t -> newValuesByStage)

    copy(basicData = newBasicData)
  }

  def scenarioData[B](scen: Scenario, p: ParamStat, values: ParamStatData): StochData = {
    val valuesByScen = scenarioData.getOrElse(scen, LinkedMap.empty)
    val newValuesByScen = valuesByScen + (p -> values)
    val newScenarioData = scenarioData + (scen -> newValuesByScen)

    copy(scenarioData = newScenarioData)
  }

  def separated(x: Boolean): StochData = copy(separated = x)

  lazy private[this] val balancedTreeIdent: List[List[(BasicScenario, Rational)]] = balancedTree(List.empty, identity)

  lazy val balancedTree: List[Scenario] = balancedTreeIdent.map(_.map(_._1)) //balancedTree(List.empty, _._1)

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

  lazy val scenarios: List[Scenario] = {
    val customTree =
      for {
        (history, bss) <- customScenarios.toList
        scen           <- bss.map(p => history :+ p._1)
        scenTree       <- balancedTree(scen, _._1)
      } yield {
        scenTree
      }

    val target = (customTree ::: balancedTree).distinct

    // TODO check why scala is faster than using spire sorting ...
    
    import Ordering.Implicits._
    target.sortBy(bsIndex)
  }

  lazy val numScenarios: Int = scenarios.size
  lazy val numStages   : Int = stages.size

  def byStage[T](scenarios: List[List[T]]): LinkedMap[Stage, LinkedMap[List[T], List[T]]] = {
    LinkedMap() ++ stages.map(x => x -> onStage(scenarios, x))
  }

  def onStage[T](scenarios: List[List[T]], t: Stage): LinkedMap[List[T], List[T]] = {
    val ind = stages.indexOf(t)
    val group = scenarios.groupByLinked(_.take(ind))
    group.map { case (k, v) => k -> v.map(x => x(ind)).distinct }
  }

  def TData: Range = 1 to numStages
  def SData: Range = 1 to numScenarios

  lazy val probabilities: List[List[Rational]] = {

    val customProbs =
      customScenarios.foldLeft(LinkedMap.empty[Stage, LinkedMap[Scenario, LinkedMap[BasicScenario, Rational]]]) {
        case (customProbs, (history, cbss)) =>
          val change =
            for {
              t       <- stages.lift(history.size)
              bss     <- basicScenarios.get(t)
              cbssBS   = cbss.unzip._1.toList
              cbssProb = cbss.unzip._2
              rbss     = bss.filterNot { case (k, _) => cbssBS.contains(k) }
              csum     = cbssProb.qsum
              newBSS   = normalize(rbss, target = 1 - csum)
            } yield {
              customProbs + (t -> LinkedMap(history -> (newBSS ++ cbss)))
            }

          change | customProbs
      }

    val balancedProbs        = balancedTreeIdent
    val balancedProbsByStage =
      for {
        (stage, bTree) <- byStage(balancedProbs)
      } yield {
        stage ->
          (for {
            (history, bss) <- bTree
          } yield {
            history.unzip._1 -> (LinkedMap() ++ bss)
          })
      }

    val probsByStage = balancedProbsByStage ++ customProbs.map {
      case (k, v) => k -> (balancedProbsByStage.getOrElse(k, LinkedMap.empty) ++ v)
    }

    val probs =
      for {
        scen <- scenarios
      } yield {
        for {
          p <- scen.zipWithIndex
          history = scen.take(p._2)
          customProb = for {
            cbss  <- customScenarios.get(history)
            cprob <- cbss.get(p._1)
          } yield {
            cprob
          }
          basicProb = for {
            t     <- stages.lift(p._2)
            bTree <- probsByStage.get(t)
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
      path <- probabilities
    } yield {
      path.map(_.toDouble).product
    }
  }

  def probabilityData2: List[Double] = {
    for {
      path <- probabilities
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
      path <- probabilities
    } yield {
      path.qproduct
    }
  }

  lazy private[stoch] val prefix: Array[Array[Scenario]] = {
    val res = Array.ofDim[Scenario](numScenarios, numStages)
    for {
      (scen, s) <- scenarios.zipWithIndex
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

  lazy private[stoch] val matrix: Array[Array[BasicScenario]] = {
    val res = Array.ofDim[BasicScenario](numScenarios, numStages)
    for {
      (scen, s) <- scenarios.zipWithIndex
      (bs, t) <- scen.zipWithIndex
    } {
      res(s)(t) = bs
    }
    res
  }

  def initMatrix(): Unit = {matrix; ()}


  // TODO take into account already sepparated models
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
        (List[SimpleData](s, t) ::: key.subscript) -> value
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
    // TODO move `if (coef == 0)...` outside the loop
    map.map { case (k, v) => k -> (if (coef == 0) Rational.zero else v / coef) }
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
  type Scenario = List[BasicScenario]

  def apply(): StochData = new StochData(Nil, LinkedMap.empty, LinkedMap.empty, LinkedMap.empty, LinkedMap.empty, LinkedMap.empty, false)

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
      case m: MultiStageStochModel => List(m.S(), m.T())
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