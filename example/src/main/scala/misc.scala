import spire.implicits._
import scalaz.syntax.show._

import amphip.dsl._
import amphip.base.LinkedMap
import amphip.data.ModelData.{DataKey, SimpleData}
import amphip.stoch.{Stage, BasicScenario, StochData}, StochData.Scenario


object misc {
  import aux._

  val S = set()
  val T = set()
  val pi = param(S)
  val p1 = param(T,S)
  val stochModel = model(minimize { p1 }).stochastic(T, S, pi)

  val (t1, t2, t3, t4) = 
    (Stage("1"), Stage("2"), Stage("3"), Stage("4"))

  val stochModelStages = stochModel.stochStages(t1, t2, t3, t4)

  //printScenarios(stochModelStages.scenariosByStage)

  val (init, a, b, c) = 
    (BasicScenario("init"), BasicScenario("a"), BasicScenario("b"), BasicScenario("c"))

  val stochModelBS = stochModelStages
    .stochBasicScenarios(t1, init -> r"1")
    .stochBasicScenarios(t2, a -> r"1/2", b -> r"1/2")
    .stochBasicScenarios(t3, a -> r"1/2", b -> r"1/2")
    .stochBasicScenarios(t4, a -> r"1/3", b -> r"1/3", c -> r"1/3")

  //printScenarios(stochModelBS.scenariosByStage)

  val stochModelCS = stochModelBS
    .stochCustomScenarios(List(init, a, a), a -> r"1/2", b -> r"1/2")
    .stochCustomScenarios(List(init, a, b), a -> r"1")
    .stochCustomScenarios(List(init, b, a), a -> r"1")

  printScenarios(stochModelCS.scenariosByStage)

  val stochModelData = stochModelCS
    .stochDefault(p1, 1.0)
    .stochBasicData(p1, t2, a, 1.1)
    .stochBasicData(p1, t2, b, 0.9)
    .stochBasicData(p1, t3, a, 1.2)
    .stochBasicData(p1, t3, b, 0.8)
    .stochBasicData(p1, t4, a, 1.3)
    .stochBasicData(p1, t4, c, 0.6)
    .stochScenarioData(p1, List(init, a, b, a), 0.9)
    .stochScenarioData(p1, List(init, b, a, a), 0.8)
    .stochScenarioData(p1, List(init, b, b, a), 0.7)

  printData(p1.name, stochModelData.stochData.paramDataST(p1))

  object aux {
    def printScenarios(ss: LinkedMap[Stage, List[Scenario]]): Unit = {
      def showScenario(s: Scenario) = s.map(_.name).mkString("[", ", ", "]")
      for {
        (t, tss) <- ss
      } {
        println(s"${t.name}:")
        println(tss.map(showScenario).mkString("\n"))
        println()
      }
    }

    def printData(pName: String, data: List[(List[SimpleData], SimpleData)]): Unit = {
      for {
        (subscript, value) <- data
        key = DataKey(pName, subscript)
      } {
        println(s"$key -> ${value.show}")
      }
      println()
    }
  }
}