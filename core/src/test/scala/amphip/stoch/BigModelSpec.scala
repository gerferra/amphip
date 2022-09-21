
package amphip.stoch

import cats.Show
import cats.syntax.show._
import cats.instances.all._
import mouse.boolean._
import spire.implicits._


import amphip.dsl._
import amphip.base._
//import amphip.sem.mathprog

object BigModelSpec extends App /* extends FunSuite */ {

  val BS = BasicScenario
  import System.{currentTimeMillis => millis}
  import StochData._

  implicit def LinkedMapShow[K: Show, V: Show]: Show[LinkedMap[K, V]] = Show.show { map =>
    map.map(_.show).toString
  }

  implicit val StageShow: Show[Stage] = Show.show(_.name)
  implicit val BSShow: Show[BasicScenario] = Show.show(_.name)

  def lazyStochData(sd: StochData) = new {

    import amphip.data.ModelData._

    import sd._

    lazy private[this] val scenariosArr: Array[Array[Scenario]] = {
      val res = Array.ofDim[Scenario](finalScenarios.size, stages.size)
      for {
        (scen, s) <- finalScenarios.zipWithIndex
      } {
        cfor(0)(_ < stages.length, _ + 1) { t =>
          res(s)(t) = scen.take(t + 1)
        }
      }
      res
    }
    
    lazy val linkData: collection.SeqView[(List[SimpleData], SimpleData), Seq[_]] = {
      val offset = separated.fold(1, 0)

      def samePrefix(s1: Int, s2: Int, t: Int): Boolean = {
        val s1_ = s1 - 1
        val s2_ = s2 - 1
        val t_ = t - 1 - offset
        scenariosArr(s1_)(t_) == scenariosArr(s2_)(t_)
      }

      val scenariosSize = finalScenarios.size
      val stagesSize = stages.size

      println("before triple iteration")

      for {
        s1 <- (1 to scenariosSize).view
        s2 <- (1 to scenariosSize).view
        t <- (1 to stagesSize).view
      } yield {
        if (s1 < 10 && s2 < 10 && t < 1) println(s"FORCING VIEW linkData: ($s1, $s2, $t)")
        List[SimpleData](s1, s2, t) -> samePrefix(s1, s2, t).fold[SimpleData](1, 0)
      }
    }

  }

  def elapsed(label: String, start: Long, end: Long): Unit = {
    println(s"$label: ${(end - start)} millis")
  }
  

  //test("Eighteen stages") {
  {
    println()
    println("Waiting for user input ...")
    scala.io.StdIn.readLine()
    println("Ready.")
    println()

    val start = millis

    val S = set
    val T = set
    val prob = param(S)
    //val link = param(S, S, T) default 0
    val ST = set(T) within S * S
    val p1 = param(T, S)
    val x1 = xvar(S, T)
    val obj = minimize(p1 * x1)

    val base = millis

    println(S.show)
    println(T.show)
    println(prob.show)
    println(ST.show)
    println(p1.show)
    println()

    val numStages = 18
    /* 10 stages
    [info] linkDataFullT: 9420 millis
    [info] linkDataFullDiagonalT: 10857 millis
    [info] linkDataOldT: 1474 millis
    [info] linkDataWithDefaultT: 1458 millis
    [info] linkDataWithDefaultDiagonalT: 1304 millis
    [info] dataT: 95137 millis
    */
    // 11 (data) OutOfMemoryError ("-XX:+UseConcMarkSweepGC", "-Xmx8G")

    println(s"""Creating "big model" for $numStages stages ...""")

    val stages = (1 to numStages).map(i => Stage(s"s$i"))
    val (a, b) = (BS("a"), BS("b"))

    val gen = millis

    val baseModel = model(obj).stochastic(T, S, prob, ST)
      .stochDefault(p1, 1.1)
      .stochStages(stages: _*)

    val assignStages = millis

    val finalModel = stages.foldLeft(baseModel) {
      (model, ti) =>
        model
          .stochBasicScenarios(ti, a -> 0.4, b -> 0.6)
          .stochBasicData(p1, ti, a, 1.2)
          .stochBasicData(p1, ti, b, 1.3)
    }

    val assignBS = millis

    val stochData = finalModel.stochData
    //val lazySD = lazyStochData(stochData)

    val stochDataT = millis

    stochData.balancedTree

    val balancedTreeT = millis

    val finalScenarios = stochData.finalScenarios

    val scenariosT = millis

    stochData.TData

    val TDataT = millis

    stochData.SData

    val SDataT = millis

    elapsed("base", start, base)
    elapsed("gen", base, gen)
    elapsed("assignStages", gen, assignStages)
    elapsed("assignBS", assignStages, assignBS)
    elapsed("stochDataT", assignBS, stochDataT)
    elapsed("balancedTreeT", stochDataT, balancedTreeT)
    elapsed("scenariosT", balancedTreeT, scenariosT)
    elapsed("TDataT", scenariosT, TDataT)
    elapsed("SDataT", TDataT, SDataT)

    /*stochData.initPrefixes()

    val initPrefixesT = millis

    stochData.linkDataFull
    
    val linkDataFullT = millis

    stochData.linkDataFullDiagonal
    
    val linkDataFullDiagonalT = millis

    stochData.linkDataWithDefault
    
    val linkDataWithDefaultT = millis

    stochData.linkDataWithDefaultDiagonal
    
    val linkDataWithDefaultDiagonalT = millis

    elapsed("initPrefixesT", getSData, initPrefixesT)
    elapsed("linkDataFullT", initPrefixesT, linkDataFullT)
    elapsed("linkDataFullDiagonalT", linkDataFullT, linkDataFullDiagonalT)
    //elapsed("linkDataT (lazy)", getSData, linkDataT)
    elapsed("linkDataWithDefaultT", linkDataFullDiagonalT, linkDataWithDefaultT)
    elapsed("linkDataWithDefaultDiagonalT", linkDataWithDefaultT, linkDataWithDefaultDiagonalT)
*/
    val startMatrix = millis

    stochData.initMatrix()

    val initMatrixT = millis

    /*stochData.linkSetData
    
    val linkSetDataT = millis*/

    //val linkSetDataBounds = stochData.linkSetDataBounds
    stochData.linkSetDataBounds

    val linkSetDataBoundsT = millis

    elapsed("initMatrixT", startMatrix, initMatrixT)
    //elapsed("linkSetDataT", initMatrixT, linkSetDataT)
    elapsed("linkSetDataBoundsT", initMatrixT, linkSetDataBoundsT)

    /*println()
    println("linkSetDataBounds:")
    linkSetDataBounds.foreach(x => println(x.show))
    println()*/

    val start2 = millis

    /*stochData.parametersData

    val parametersDataT = millis*/

    stochData.parametersDataBounds

    val parametersDataBoundsT = millis

    //elapsed("parametersDataT", start2, parametersDataT)
    elapsed("parametersDataBoundsT", start2, parametersDataBoundsT)

    val start3 = millis

    stochData.finalProbabilities

    val probabilitiesT = millis

    val probData = stochData.probabilityData
    
    val probabilityDataT = millis

    /*val probData2 = stochData.probabilityData2
    
    val probabilityData2T = millis*/
    

    elapsed("probabilitiesT", start3, probabilitiesT)
    elapsed("probabilityDataT", probabilitiesT, probabilityDataT)
    //elapsed("probabilityData2T", probabilityDataT, probabilityData2T)
    
    /*def fixPDataSum(iter: Int): Double = {
        var pDataSum = probData.sum
        cfor(1)(_ < iter && pDataSum != 1, _ + 1) { i =>
          pDataSum = probData.map(_ / pDataSum).sum
        }
        pDataSum
    }*/

    val start4 = millis 

    val pDataSum = probData.sum//fixPDataSum(1)// probData.sum

    val probDataSumT = millis

    /*val pDataSum2 = probData2.qsum

    val probData2SumT = millis*/
    
    elapsed("probDataSumT", start4, probDataSumT)
    //elapsed("probData2SumT", probDataSumT, probData2SumT)

    println()
    println("probData:")
    println(probData.take(10).mkString("\n"))
    println()

    println("probData.sum:  " + pDataSum)
    //println("probData2.sum: " + pDataSum2)
    println()

    println()
    /*println("Generating mip model (data) ...")

    val startData = millis
    
    val mip = finalModel.mip

    val mipT = millis

    elapsed("mipT", startData, mipT)
    elapsed("total", start, startData)*/

    println("finalScenarios:")
    println(finalScenarios.take(10).mkString("", "\n", "\n"))


    /*val start5 = millis

    val data = mip.data
    val dataSection = mathprog.genData(data)

    val dataSectionT = millis
    
    val mipModel = mip.model
    val modelSection = mathprog.genModel(mipModel)

    val modelSectionT = millis

    val dataPath = Path("src") / "test" / "resources" / "big-model-spec.dat"
    dataPath.write(dataSection)

    val writeDataSectionT = millis

    val modelPath = Path("src") / "test" / "resources" / "big-model-spec.mod"
    modelPath.write(modelSection)

    val writeModelSectionT = millis

    elapsed("dataSectionT", start5, dataSectionT)
    elapsed("modelSectionT", dataSectionT, modelSectionT)
    elapsed("writeDataSectionT", dataSectionT, writeDataSectionT)
    elapsed("writeModelSectionT", writeDataSectionT, writeModelSectionT)

    println()
    println(s"dataPath: ${dataPath.path} (${dataPath.size})")
    println(s"modelPath: ${modelPath.path} (${modelPath.size})")*/

  }
  println("Waiting for user input ...")
  scala.io.StdIn.readLine()
  println("Done.")
}