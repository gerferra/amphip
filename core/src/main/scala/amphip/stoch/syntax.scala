package amphip.stoch

import scalaz.std.option._, optionSyntax._

import spire.math._, spire.implicits._

import amphip.base.LinkedMap
import amphip.base.implicits._
import amphip.model.ast._
import amphip.model.dsl._
import amphip.model.collect
import amphip.data._
import amphip.data.dsl._
import amphip.data.ops._
import amphip.stoch.instances._
import amphip.solver

object syntax extends AllSyntax

trait AllSyntax {

  implicit class ModelWithDataStochSyntax[M](m: M)(implicit conv: M => ModelWithData) {
    private def update(model: Model): ModelWithData = m.copy(model = model)

    def stochastic(T: SetStat, S: SetStat, prob: ParamStat): StochModel = {
      val prevStat = List[Stat](S, T, prob).flatMap(collect(_, { case s: Stat => s }))
      MultiStageStochModel(update(Model((prevStat ::: m.model.statements).distinct)), StochData(), T, S, prob, STAdapter(T, S))
    }

    def stochastic(T: SetStat, S: SetStat, prob: ParamStat, link: SetStat): StochModel = {
      val prevStat = List[Stat](S, T, prob, link).flatMap(collect(_, { case s: Stat => s }))
      MultiStageStochModel(update(Model((prevStat ::: m.model.statements).distinct)), StochData(), T, S, prob, CompressedNAMode(link))
    }

    def stochastic(T: SetStat, S: SetStat, prob: ParamStat, link: ParamStat, naForm: NAForm): StochModel = {
      val prevStat = List[Stat](S, T, prob, link).flatMap(collect(_, { case s: Stat => s }))
      MultiStageStochModel(update(Model((prevStat ::: m.model.statements).distinct)), StochData(), T, S, prob, DenseNAMode(link, naForm))
    }

    /**
     * Specifies that the model is a two-stage stochastic model with the 
     * scenarios set `S` and the probabilities parameter `prob`.
     */
    def stochastic(S: SetStat, prob: ParamStat): StochModel = {
      val prevStat = List[Stat](S, prob).flatMap(collect(_, { case s: Stat => s }))
      val base = TwoStageStochModel(update(Model((prevStat ::: m.model.statements).distinct)), StochData(), S, prob)
      base.stochStages(Stage("single"))
    }

  }

  implicit class StochModelSyntax[M](m: M)(implicit conv: M => StochModel) {
    private def update(model: ModelWithData): StochModel = (m: StochModel) match {
      case m: TwoStageStochModel => m.copy(model = model)
      case m: MultiStageStochModel => m.copy(model = model)
    }

    def relax(xvar: VarStat): StochModel = update(m.model.relax(xvar))

    def relax: StochModel = update(m.model.relax)

    def replace[A](target: A, replacement: A): StochModel = 
      amphip.model.replace(m: StochModel, target, replacement)

    def statements: List[Stat] = m.model.statements ++ nonanticipativityConstraints

    def variables: List[VarStat] = m.model.variables

    def parameters: List[ParamStat] = m.model.parameters

    def sets: List[SetStat] = m.model.sets

    def objective: ObjectiveStat = m.model.objective

    def constraints: List[ConstraintStat] = m.model.constraints ++ nonanticipativityConstraints

    def param(name: String): ParamStat = m.model.param(name)

    def xvar(name: String): VarStat = m.model.xvar(name)

    def set(name: String): SetStat = m.model.set(name)

    def ctr(name: String): ConstraintStat = m.model.ctr(name)

    def +:(stat: Stat): StochModel = update(stat +: m.model)

    def :+(stat: Stat): StochModel = update(m.model :+ stat)

    def ++:(stats: List[Stat]): StochModel = update(stats ++: m.model)
    
    def :++(stats: List[Stat]): StochModel = update(m.model :++ stats)

    ////

    def paramData[B](decl: ParamStat, values: B*)(implicit DataOp: DataOp[ParamStat, B]): StochModel = update(m.model.paramData(decl, values: _*))

    def paramDataList[B](list: List[(ParamStat, List[B])])(implicit DataOp: DataOp[ParamStat, B]): StochModel = update(m.model.paramDataList(list))

    def setData[B](decl: SetStat, values: B*)(implicit DataOp: DataOp[SetStat, B]): StochModel = update(m.model.setData(decl, values: _*))

    def setDataList[B](list: List[(SetStat, List[B])])(implicit DataOp: DataOp[SetStat, B]): StochModel = update(m.model.setDataList(list))

    def eval[A, B](expr: A)(implicit Eval: amphip.data.eval.Eval[A, B]): B = m.model.eval(expr)

    def solve = {
      solver.GLPSOL.solve(m.mip)
    }

    ////

    def optT: Option[SetStat] = (m: StochModel) match {
      case m: MultiStageStochModel => m.T.some
      case _ => none
    }

    def isDeterministic(indexing: IndExpr): Boolean = !indexing.entries.exists(_.set == m.S())

    def isStochastic(indexing: IndExpr): Boolean = (m: StochModel) match {
      case m: TwoStageStochModel =>
        indexing.entries.exists(_.set == m.S())

      case m: MultiStageStochModel =>
        val dependsOnS = indexing.entries.exists(_.set == m.S())
        val dependsOnT = indexing.entries.exists(_.set == m.T())
        dependsOnS && dependsOnT
    }

    def isDeterministic(xvar: VarStat): Boolean = xvar.domain.map(isDeterministic) | true

    def isStochastic(xvar: VarStat): Boolean = xvar.domain.map(isStochastic) | false

    def isDeterministic(param: ParamStat): Boolean = param.domain.map(isDeterministic) | true

    def isStochastic(param: ParamStat): Boolean = param.domain.map(isStochastic) | false

    def deterministicVariables: List[VarStat] = variables.filter(isDeterministic)

    def stochasticVariables: List[VarStat] = variables.filter(isStochastic)

    def deterministicParameters: List[ParamStat] = parameters.filter(isDeterministic)

    def stochasticParameters: List[ParamStat] = parameters.filter(isStochastic)

    def separate: ModelWithData = amphip.stoch.separate(m)

    def nonanticipativityConstraints: List[ConstraintStat] = stochasticVariables.flatMap(nonanticipativity)

    def nonanticipativity(xvar: VarStat): Option[ConstraintStat] = {
      import amphip.stoch.nonanticipativity._
      (m: StochModel) match {          
        case m: MultiStageStochModel => m.naMode match {
          case na: STAdapter => amphip.stoch.nonanticipativity(xvar, m.T, m.S, na)
          case CompressedNAMode(link) => amphip.stoch.nonanticipativity(xvar, m.T, m.S, link)
          case DenseNAMode(link, NAForm.X) => nonanticipativityTemplate(xvar, m.T, m.S, link, nonanticipativityXFunc)
          case DenseNAMode(link, NAForm.Z) => nonanticipativityTemplate(xvar, m.T, m.S, link, nonanticipativityZFunc)
        }
        case _ => none
      }
    }

    //// 

    private def update(stochData: StochData): StochModel = (m: StochModel) match {
      case m: TwoStageStochModel   => m.copy(stochData = stochData)
      case m: MultiStageStochModel => m.copy(stochData = stochData)
    }

    import StochData._

    def stochStages(ts: Stage*): StochModel = update(m.stochData.stages(ts: _*))
    def stochStages                         = m.stochData.stages

    def stochBasicScenarios(t: Stage, bss: (BasicScenario, Rational)*): StochModel = {
      update(m.stochData.basicScenarios(t, bss: _*))
    }

    def stochCustomScenarios(history: Scenario, changes: (BasicScenario, Rational)*): StochModel = {
      update(m.stochData.customScenarios(history, changes: _*))
    }

    def finalScenarios: List[Scenario] = m.stochData.finalScenarios
    def scenariosByStage: LinkedMap[Stage, List[Scenario]] = m.stochData.scenariosByStage
    def scenarios: List[Scenario] = m.stochData.scenarios

    def finalProbabilities: List[List[Rational]] = m.stochData.finalProbabilities

    /*
      Defines the path probabilities of the final scenarios.
      Uses `stochCustomScenarios` accordingly to match the specified path probability
      of each scenario. 
     */
    def stochProbabilities(sp: Iterable[(Scenario, Rational)]): StochModel = {
      // history size on each stage
      val hSize = m.stochStages.indices

      // StochData after assignig custom probabilities to every possible history
      // and basic scenarios 
      val newStochData = 
        hSize.foldLeft(m.stochData) { (stochData, th) =>
          // groups scenarios based on the current `history size`
          val hGroup = sp.groupByLinked { case (scen, _) => scen.take(th) }

          // StochData after assigning custom probabilities to every history on 
          // this stage
          val newStochData = 
           hGroup.foldLeft(stochData) { case (stochData, (history, group)) =>
              val historyProb = group.unzip._2.qsum

              // computes basic scenarios corresponding to `history`.
              // all this groups have the same `history` and only differ on the last
              // basic scenario.
              val onStage = group.groupByLinked { case (scen, _) => scen.take(th+1) }
              val onStageProbs = onStage.mapValues(_.unzip._2.qsum)
              
              // divides between the `historyProb` to transform the `path probability`
              // to the basic scenario probability
              val bssProbs = 
                for {
                  (scen, prob) <- onStageProbs
                  bs           <- scen.lastOption
                } yield {
                  bs -> prob / historyProb
                }

              stochData.customScenarios(history, bssProbs.toSeq:_*)
            }
          newStochData
        }

      update(newStochData)
    }

    def stochDefault[B](p: ParamStat, values: B*)(implicit ev: DataOp[ParamStat, B]): StochModel = {
      requireStochastic(p, m)
      val newData = ev.data(asDet(p, m), values.toList)(m.model.data)
      val pData = filter(newData, p).params
      update(m.stochData.default(p, pData))
    }

    def stochBasicData[B](p: ParamStat, t: Stage, bs: BasicScenario, values: B*)(implicit ev: DataOp[ParamStat, B]): StochModel = {
      requireStochastic(p, m)
      val newData = ev.data(asDet(p, m), values.toList)(m.model.data)
      val pData = filter(newData, p).params
      update(m.stochData.basicData(t, bs, p, pData))
    }

    def stochScenarioData[B](p: ParamStat, scen: Scenario, values: B*)(implicit ev: DataOp[ParamStat, B]): StochModel = {
      requireStochastic(p, m)
      val newData = ev.data(asDet(p, m), values.toList)(m.model.data)
      val pData = filter(newData, p).params
      update(m.stochData.scenarioData(scen, p, pData))
    }

    def stochScenarioData[B](p: ParamStat, list: Iterable[(Scenario, Seq[B])])(implicit ev: DataOp[ParamStat, B]): StochModel = {
      list.foldLeft(m: StochModel) { case (model, (scen, values)) =>
        model.stochScenarioData(p, scen, values: _*)
      }
    }

    def alreadySeparated: StochModel = update(m.stochData.separated(true))
    
    def mip: ModelWithData = {
      val model0 = m: StochModel

      import model0.{stochData, S, pi}

      val model1 =
        model0 match {
          case model0: TwoStageStochModel =>
            model0
              .setData(S, stochData.SData)
              .paramData(pi, stochData.probabilityData) 
              .paramDataList(stochData.parametersData.map {
                case (param, data) => param -> data.map {
                  case (subscript, data) => subscript.drop(1) -> data // removes stage index in two-stage model
                }
              })

          case model0: MultiStageStochModel =>
            import model0.{T, naMode}

            /* 
              XXX review `nonanticipativityConstraints` method: 
              non-anticipativity can be incorporated in different ways 
              depending on the `naMode` (eg. with pairs of adapted variables 
              and constraints)
            */
            val model01 = (model0 :++ nonanticipativityConstraints)
              .setData(T, stochData.TData)

            naMode match {
              case DenseNAMode(link, _) => 
                model01
                  .setData(S, stochData.SData)
                  .paramData(pi, stochData.probabilityData) 
                  .paramData(link, stochData.linkData)
                  .paramDataList(stochData.parametersData)

              case CompressedNAMode(link) => 
                model01
                  .setData(S, stochData.SData)
                  .paramData(pi, stochData.probabilityData) 
                  .setData(link, stochData.linkSetDataBounds.map {
                    case (t, tupList) => t -> tupList.map(tup => List(tup._1, tup._2))
                  })
                  .paramDataList(stochData.parametersData)

              case na: STAdapter =>
                import amphip.model.{replace => replacef}
                import na.{ST, pred, H, ancf}

                val adaptedParams = model01.stochasticParameters.flatMap(na.adaptParam)
                val st_params     = adaptedParams.unzip._2
                  
                val model02 = (ancf :: st_params) ++: model01

                val SA      = S default ST(H)
                val model03 = model02.replace(S, SA)
                val pA      = model03.pi // pi using the new S

                val model04 = model03
                  .setData(ST, stochData.STData)
                  .paramData(pred, stochData.predecessorsData)
                  .paramData(pA  , stochData.probabilityData)

                /* 
                  Replaces parameter with version which defaults to adaptation 
                  without separated scenarios, and specify the parameter values
                  only on adapted version.
                 */
                val model05 = model04.stochasticParameters.zip(adaptedParams)
                  .foldLeft(model04) { case (model, (param, (paramA0, st_param))) =>
                    val paramA1 = replacef(paramA0, S, SA)
                    model
                      .replace(param, paramA1)
                      .paramData(st_param, stochData.paramDataST(param))
                  }                

                model05
            }
        }

      model1.model
    }

    def mip2: ModelWithData = ((m: StochModel): @unchecked) match {
      case model0 @ MultiStageStochModel(_, stochData, T @ _, S @ _, _, na: STAdapter) =>
        import amphip.model.{replace => replacef}
        import na.{ST, pred, H, ancf}

        // adds ST, H, pred, ancf which are collected from the
        // non anticipativity constraints
        val model1 = (model0 :++ nonanticipativityConstraints)

        val adaptedParams = model1.stochasticParameters.flatMap(na.adaptParam)
        val st_params     = adaptedParams.unzip._2

        // ancf is added to move before the parameters declarations
        val model2 = (ancf :: st_params) ++: model1

        val SA     = S default ST(H)
        val model3 = model2.replace(S, SA)
        val piA    = model3.pi // pi using SA

        val model4 = model3
          .setData(T , stochData.TData)
          .setData(ST, stochData.STData)
          .paramData(pred, stochData.predecessorsData)
          .paramData(piA , stochData.probabilityData)

        val model5 = model4.stochasticParameters.zip(adaptedParams)
          .foldLeft(model4) { case (model, (param, (paramA0, st_param))) =>
            val paramA1 = replacef(paramA0, S, SA)
            model
              .replace(param, paramA1)
              .paramData(st_param, stochData.paramDataST(param))
          }                

        model5.model
    }

    def data = mip.data
  }

}