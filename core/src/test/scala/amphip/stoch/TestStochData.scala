package amphip.stoch

import spire.implicits._

import org.junit.Assert._
import org.junit.Test

import amphip.dsl._
import amphip.model.ast._
import amphip.data._

class TestStochData {

  @Test
  def testModel1(): Unit = {

    val S = set("S")
    val T = set("T")
    val prob = param("prob", S)
    //val link = param("link", S, S, T)
    val link = set("link", T)
    val p1 = param("p1", S, T)
    val p2 = param("p2", S, T)
    val p3 = param("p3", S, T)

    val model = Model(List(p1, p2, p3)).stochastic(S, T, prob, link)

    val model1 = model
      .stochDefault(p1, 1)
      .stochDefault(p2, 8)
      .stochDefault(p3, 0)

    val (t1, t2, t3) = (Stage("1"), Stage("2"), Stage("3"))

    val model2 = model1.stochStages(t1, t2, t3) // to stablish the order

    val BS = BasicScenario

    val (low1, med1, high1) = (BS("Low1"), BS("Medium1"), BS("High1"))
    val (low2, high2)       = (BS("Low2"), BS("High2"))
    val (low3, high3)       = (BS("Low3"), BS("High3"))

    // 3 * 2 * 2 = 12 scenarios
    val model3 = model2
      .stochBasicScenarios(t1, low1 -> r"0.1", med1  -> r"0.5", high1 -> r"0.3")
      .stochBasicScenarios(t2, low2 -> r"0.5", high2 -> r"0.3")
      .stochBasicScenarios(t3, low3 -> r"0.2", high3 -> r"0.5")

    val model4 = model3
      .stochBasicData(t1, low1,              p1, 0)
      .stochBasicData(t1, high1,             p1, 2)
      .stochBasicData(t2, high2,             p1, 0.3)
      .stochBasicData(t2, low2,              p2, 0.5)
      .stochBasicData(t2, BS("nonexistant"), p2, 0.5)

    // changes probability of (low1, low2, high3), adds (low1, low2, new3). total = 13 scenarios 
    val model5 = model4
      .stochCustomScenarios(List(low1, low2), high3 -> r"0.6", BS("New3") -> r"0.1")

    val model6 = model5
      .stochScenarioData(List(low1, low2,  high3),        p1, 2.5)
      .stochScenarioData(List(low1, high2, BS("High3a")), p1, 2.3)
      .stochScenarioData(List(low1, high2, BS("High3b")), p1, 2.4)

    val stochData = model6.stochData

    val TData = stochData.TData
    val SData = stochData.SData

    println(s"TData: $TData")
    println(s"SData: $SData")

    val scenarios = stochData.scenarios

    println()
    println("scenarios:")
    println(scenarios.mkString("", "\n", "\n"))

    println("probabilities:")
    val probabilities = stochData.probabilities
    println(probabilities.mkString("", "\n", "\n"))

    println()
    println("mix:")
    val mix = scenarios.zip(probabilities).map { case (ss, ps) => ss.zip(ps.map(_.toDouble)) }
    println(mix.mkString("", "\n", "\n"))

    val probData = stochData.probabilityData
    println(s"probabilityData: $probData")
    println(s"probabilityData.sum: ${probData.sum}")
    println()

    val p1Data = stochData.paramData(p1).toMap
    val p1Val = p1Data(List(2, 3))
    assertEquals(p1Val, SimpleNum(2.5))
        
    val p1ModelData = StochData.filter(model6.data, p1).params
    println("p1ModelData:")
    println(amphip.sem.mathprog.genData.getParamData(p1ModelData))
    println()

    val linkModelData = StochData.filter(model6.data, link).sets
    println("linkModelData:")
    println(amphip.sem.mathprog.genData.getSetData(linkModelData))
    println()

  }

  //@Test
  def testModel2(): Unit = {

    val stocks = "stocks"
    val bonds = "bonds"

    val H = param("H") // horizon of stages

    val nS = param("nS")
    val S = set("S") default (1 to nS) // scenarios. 
    val s = dummy("s")
    val p = param("p", S)

    val T = set("T") default (1 to H) // stages
    val t = dummy("t")

    val G = param("G") // goal money
    val b = param("b") // initial money

    val I = set("I") default List(stocks, bonds) // investments. 
    val i = dummy("i")

    val ret = param("ret", S, T, I) // random returns of investment

    val x = xvar("x", S, T, I) >= 0 // investment decisions

    val q = param("q") // excess utility rate
    val r = param("r") // deficit utility rate
    val y = xvar("y", S) >= 0 // excess below G 
    val w = xvar("w", S) >= 0 // deficit over G

    val maxUtility = maximize {
      sum(s in S)(p(s) * (-r * w(s) + q * y(s)))
    }

    val initial = st(s in S) { sum(i in I)(x(s, 1, i)) === b }

    val balance = st(s in S, t in (2 to H)) {
      sum(i in I)(ret(s, t - 1, i) * x(s, t - 1, i)) === sum(i in I)(x(s, t, i))
    }

    val close = st(s in S) {
      sum(i in I)(ret(s, H, i) * x(s, H, i)) + w(s) - y(s) === G
    }

    val financeMIP = model(maxUtility,
      initial,
      balance,
      close)

    //val link = param("link", S, S, T)
    val link = set("link", T)

    val HVal = 3

    val financeStoch = financeMIP.stochastic(S, T, p, link)
      .paramData(H, HVal)
      .paramData(b, 55000)
      .paramData(G, 80000)
      .paramData(q, 1)
      .paramData(r, 1)

    val BS = BasicScenario
    val (low, high) = (BS("Low"), BS("High"))

    val stages = (1 to HVal).map(i => Stage(i.toString))

    val financeStoch1 = financeStoch.stochStages(stages: _*)

    val financeStoch2 = stages.foldLeft(financeStoch1) {
      (financeStoch1, ti) =>
        financeStoch1
          .stochBasicScenarios(ti, low -> 0.5, high -> 0.5)
          .stochBasicData(ti, low, ret,
            List(stocks) -> 1.06,
            List(bonds) -> 1.12)
          .stochBasicData(ti, high, ret,
            List(stocks) -> 1.25,
            List(bonds) -> 1.14)
    }

    val financeStoch3 = financeStoch2.alreadySeparated
    
    val stochData = financeStoch3.stochData

    val TData = stochData.TData
    val SData = stochData.SData

    println(TData)
    println(SData)

    val probabilities = stochData.probabilities
    val probData = stochData.probabilityData

    println(probData.sum)

    println(probabilities.mkString("\n"))
    println(probData)

    //val linkData = stochData.linkData

    //println(linkData.mkString("\n"))

    val scenarios = stochData.scenarios

    println(scenarios.mkString("\n"))

    val retData = stochData.paramData(ret)

    println(retData.mkString("\n"))

    // XXX add test for tree stages and two stages in basicScnarios and stage 3 only in customScenarios

    val finance = financeStoch3.paramData(nS, SData.size)

    println(amphip.sem.mathprog.genModel(finance.mip.model))
    println(amphip.sem.mathprog.genData(finance.data))

    println(finance.solve)

  }

}