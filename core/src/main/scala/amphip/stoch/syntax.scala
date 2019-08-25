package amphip.stoch

import scalaz.std.option._, optionSyntax._

import spire.math._

import amphip.model.ast._
import amphip.model.dsl._
import amphip.data._
import amphip.data.dsl._
import amphip.data.ops._
import amphip.collect
import amphip.solver

object syntax extends AllSyntax

trait AllSyntax {

  implicit class ModelWithDataStochSyntax[M](val m: M)(implicit conv: M => ModelWithData) {
    private def update(model: Model): ModelWithData = m.copy(model = model)

    def stochastic(S: SetStat, T: SetStat, prob: ParamStat, link: SetStat): StochModel = {
      val prevStat = List[Stat](S, T, prob, link).flatMap(collect(_, { case s: Stat => s }))
      MultiStageStochModel(update(Model((prevStat ::: m.model.statements).distinct)), StochData(), S, T, prob, CompressedNAMode(link))
    }

    def stochastic(S: SetStat, T: SetStat, prob: ParamStat, link: ParamStat, naForm: NAForm): StochModel = {
      val prevStat = List[Stat](S, T, prob, link).flatMap(collect(_, { case s: Stat => s }))
      MultiStageStochModel(update(Model((prevStat ::: m.model.statements).distinct)), StochData(), S, T, prob, DenseNAMode(link, naForm))
    }

    def stochastic(S: SetStat, prob: ParamStat): StochModel = {
      val prevStat = List[Stat](S, prob).flatMap(collect(_, { case s: Stat => s }))
      TwoStageStochModel(update(Model((prevStat ::: m.model.statements).distinct)), StochData(), S, prob)
    }

  }

  implicit class StochModelSyntax[M](val m: M)(implicit conv: M => StochModel) {
    private def update(model: ModelWithData): StochModel = (m: StochModel) match {
      case m: TwoStageStochModel => m.copy(model = model)
      case m: MultiStageStochModel => m.copy(model = model)
    }

    def relax(xvar: VarStat): StochModel = update(m.model.relax(xvar))

    def relax: StochModel = update(m.model.relax)

    def replace[A: Manifest](target: A, replacement: A): StochModel = update(m.model.replace(target, replacement))

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

    def eval[A, B](expr: A)(implicit Eval: amphip.eval.Eval[A, B]): B = m.model.eval(expr)

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

    def separate: ModelWithData = amphip.separate(m)

    def nonanticipativityConstraints: List[ConstraintStat] = stochasticVariables.flatMap(nonanticipativity)

    def nonanticipativity(xvar: VarStat): Option[ConstraintStat] = {
      import amphip.stoch.nonanticipativity._
      (m: StochModel) match {          
        case m: MultiStageStochModel => m.naMode match {
          case CompressedNAMode(link) => amphip.stoch.nonanticipativity(xvar, m.S, m.T, link)
          case DenseNAMode(link, NAForm.X) => nonanticipativityTemplate(xvar, m.S, m.T, link, nonanticipativityXFunc)
          case DenseNAMode(link, NAForm.Z) => nonanticipativityTemplate(xvar, m.S, m.T, link, nonanticipativityZFunc)
        }
        case _ => none
      }
    }

    //// 

    private def update(stochData: StochData): StochModel = (m: StochModel) match {
      case m: TwoStageStochModel => m.copy(stochData = stochData)
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

    def stochDefault[B](p: ParamStat, values: B*)(implicit ev: DataOp[ParamStat, B]): StochModel = {
      requireStochastic(p, m)
      val newData = ev.data(asDet(p, m), values.toList)(m.model.data)
      val pData = filter(newData, p).params
      update(m.stochData.default(p, pData))
    }

    def stochBasicData[B](t: Stage, bs: BasicScenario, p: ParamStat, values: B*)(implicit ev: DataOp[ParamStat, B]): StochModel = {
      requireStochastic(p, m)
      val newData = ev.data(asDet(p, m), values.toList)(m.model.data)
      val pData = filter(newData, p).params
      update(m.stochData.basicData(t, bs, p, pData))
    }

    def stochScenarioData[B](scen: Scenario, p: ParamStat, values: B*)(implicit ev: DataOp[ParamStat, B]): StochModel = {
      requireStochastic(p, m)
      val newData = ev.data(asDet(p, m), values.toList)(m.model.data)
      val pData = filter(newData, p).params
      update(m.stochData.scenarioData(scen, p, pData))
    }

    def alreadySeparated: StochModel = update(m.stochData.separated(true))

    def data: ModelData = {
      val baseModel = m: StochModel

      import baseModel.{stochData, S, p}

      // TODO handle the case when the values are specified without using the stochastic data api?
      val model1 = baseModel.setData(S, stochData.SData)

      val model2 =
        model1 match {
          case model1: MultiStageStochModel =>
            import model1.{T, naMode}

            val m = model1.setData(T, stochData.TData)

            naMode match {
              case CompressedNAMode(link) => 
                m.setData(link, stochData.linkSetDataBounds.map {
                  case (t, tupList) => t -> tupList.map(tup => List(tup._1, tup._2))
                })
              case DenseNAMode(link, _) => 
                m.paramData(link, stochData.linkData)
            }

          case _ => model1
        }

      val model3 = model2
        .paramData(p, stochData.probabilityData)
        .paramDataList(stochData.parametersData)

      model3.model.data
    }

    def mip: ModelWithData = {
      val basicModel = m.model.model
      val data       = m.data
      val after      = nonanticipativityConstraints
      val stochModel = Model(basicModel.statements ::: after)
      ModelWithData(stochModel, data)
    }
  }

}