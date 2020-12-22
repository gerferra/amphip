package amphip.stoch

import scala.collection.mutable.ListBuffer

import java.util.concurrent.TimeUnit

import spire.implicits._

import org.openjdk.jmh.annotations._

import amphip.dsl._
import amphip.stoch.StochDataBench._
import amphip.model.ast._
import amphip.data.ModelData._

object StochDataBench {
  val BS = BasicScenario
}

/* bench/jmh:run -wi 10 -i 10 -f 1 -prof gc  -rf csv StochDataBench */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class StochDataBench {

  @Param(Array("8", "12", "15"))
  var totalStages: Int = 0

  var testModel: StochModel = null

  var stochData: StochData = null

  var p1: ParamStat = null

  ////
  var SDataList: List[Int] = Nil 
  var TDataList: List[Int] = Nil   
  ////

  @Setup
  def setup(): Unit = {

    val S = set
    val T = set
    val prob = param(S)
    //val link = param(S, S, T) default 0
    val ST = set(T) within S * S
    val p1 = param(T, S)
    val x1 = xvar(T, S)
    val obj = minimize(p1 * x1)
    val mipModel = model(obj)
    val stochModel = mipModel.stochastic(T, S, prob, ST)

    val stages = (1 to totalStages).map(i => Stage(s"s$i"))
    val (a, b) = (BS("a"), BS("b"))

    val stochModelDefData = stochModel
      .stochDefault(p1, 1.1)
      .stochStages(stages: _*)

    val stochModelBasicData = stages.foldLeft(stochModelDefData) {
      (model, ti) =>
        model
          .stochBasicScenarios(ti, a -> 0.4, b -> 0.6)
          .stochBasicData(p1, ti, a, 1.2)
          .stochBasicData(p1, ti, b, 1.3)
    }

    testModel = stochModelBasicData

    val sdata = testModel.stochData
    sdata.initMatrix()
    stochData = sdata

    StochDataBench.this.p1 = testModel.param("p1")

    SDataList = stochData.SData.toList
    TDataList = stochData.TData.toList
  }

  @Benchmark
  def lSetDataBounds = {
    stochData.linkSetDataBounds
  } 

  @Benchmark
  def pDataOrigOnePass = {
    paramDataOrigOnePass(p1)
  }

  @Benchmark
  def pData = {
    stochData.paramData(p1)
  }

  @Benchmark
  def pDataCacheList = {
    paramDataCacheList(p1)
  }

  @Benchmark
  def pDataCacheBuffer = {
    paramDataCacheBuffer(p1)
  }

  @Benchmark
  def pDataBoundsTwoPass = {
    paramDataBoundsTwoPass(p1)
  }

  @Benchmark
  def pDataBounds = {
    stochData.paramDataBounds(p1)
  }


  ///////

  def paramDataOrigOnePass(param: ParamStat): List[(List[SimpleData], SimpleData)] = {
    val sdata = stochData
    import sdata._

    val scenariosData =
      for {
        (scen, s) <- finalScenarios.zipWithIndex
        pair <- scen.zipWithIndex
        bs = pair._1
        t = pair._2
        cData = for {
          cbss <- scenarioData.get(scen.take(t + 1))
          data <- cbss.get(param)
        } yield {
          data
        }
        bData = for {
          stage <- stages.lift(t)
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
        (List[SimpleData](t + 1, s + 1) ::: key.subscript) -> value
      }

    scenariosData
  }

  

  def paramDataCacheList(param: ParamStat): List[(List[SimpleData], SimpleData)] = {
    val sdata = stochData
    import sdata._

    val scenariosData =
      for {
        s <- SDataList
        t <- TDataList
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

    scenariosData
  }

  def paramDataCacheBuffer(param: ParamStat): List[(List[SimpleData], SimpleData)] = {
    val sdata = stochData
    import sdata._

    val res = ListBuffer.empty[(List[SimpleData], SimpleData)]
    cfor(0)(_ < numScenarios, _ + 1) { s_ =>
      cfor(0)(_ < numStages, _ + 1) { t_ =>
        val scenPrefix = prefix(s_)(t_)
        val bs = matrix(s_)(t_)
        val cData = for {
          cbss <- scenarioData.get(scenPrefix)
          data <- cbss.get(param)
        } yield {
          data
        }
        val bData = for {
          stage <- stages.lift(t_)
          bss <- basicData.get(stage)
          ds <- bss.get(bs)
          data <- ds.get(param)
        } yield {
          data
        }
        val defaultData = defaults.get(param)
        
        val stValues = 
          for { 
            pData <- cData.orElse(bData).orElse(defaultData).toList
            (key, value) <- pData
          } yield {
            (List[SimpleData](t_ + 1, s_ + 1) ::: key.subscript) -> value
          }
        res ++= stValues
      }
    } 
    
    res.toList
  }

  def paramDataBoundsTwoPass(param: ParamStat): List[(List[SimpleData], SimpleData)] = {
    val sdata = stochData
    import sdata._

    val scenariosData =
      for {
        (t, tup) <- stochData.linkSetDataBounds(includeSingleRanges = true)
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
      } yield {
        for {
          data <- cData.orElse(bData).orElse(defaultData)
        } yield {
          (t, s1, s2) -> data
        }
      }

    val data =
      for {
        scenData <- scenariosData
        ((t, s1, s2), pData) <- scenData.toList
        (key, value) <- pData
      } yield {
        (List[SimpleData](t, s1, s2) ::: key.subscript) -> value
      }

    data
  }

}