package amphip.data.dsl

import scala.language.implicitConversions

import scalaz.std.list.listSyntax._

import amphip.model.ast._
import amphip.model.dsl._
import amphip.data._
import amphip.data.dsl.ops._
import amphip.collect
import amphip.solver

object syntax extends Syntax

trait Syntax {

  implicit def ModelAsModelWithData(m: Model): ModelWithData = ModelWithData(m, ModelData())

  implicit class ModelWithDataSyntax[M](val m: M)(implicit conv: M => ModelWithData) {
    private def update(model: Model): ModelWithData = m.copy(model = model)

    def relax(xvar: VarStat): ModelWithData = update(m.model.relax(xvar))
    def relax: ModelWithData = update(m.model.relax)

    def replace[A: Manifest](target: A, replacement: A): ModelWithData = update(m.model.replace(target, replacement))

    def statements: List[Stat] = m.model.statements
    def variables: List[VarStat] = m.model.variables
    def parameters: List[ParamStat] = m.model.parameters
    def sets: List[SetStat] = m.model.sets
    def objective: ObjectiveStat = m.model.objective
    def constraints: List[ConstraintStat] = m.model.constraints

    def param(name: String): ParamStat = m.model.param(name)
    def xvar(name: String): VarStat = m.model.xvar(name)
    def set(name: String): SetStat = m.model.set(name)
    def ctr(name: String): ConstraintStat = m.model.ctr(name)

    def +:(stat: Stat): ModelWithData = update(stat +: m.model)
    def :+(stat: Stat): ModelWithData = update(m.model :+ stat)
    def ++(stats: List[Stat]): ModelWithData = update(m.model ++ stats)

    ////

    def paramData[B](decl: ParamStat, values: B*)(implicit DataOp: DataOp[ParamStat, B]): ModelWithData = {
      val newData = DataOp.data(decl, values.toList)(m.data)
      ModelWithData(m.model, m.data + newData)
    }
    def paramDataList[B](list: List[(ParamStat, List[B])])(implicit DataOp: DataOp[ParamStat, B]): ModelWithData = {
      list.foldLeft(m: ModelWithData) {
        case (model, (decl, data)) => data.toNel.fold(model)(nel => model.paramData(decl, nel.list: _*))
      }
    }

    def setData[B](decl: SetStat, values: B*)(implicit DataOp: DataOp[SetStat, B]): ModelWithData = {
      val newData = DataOp.data(decl, values.toList)(m.data)
      ModelWithData(m.model, m.data + newData)
    }
    def setDataList[B](list: List[(SetStat, List[B])])(implicit DataOp: DataOp[SetStat, B]): ModelWithData = {
      list.foldLeft(m: ModelWithData) {
        case (model, (decl, data)) => data.toNel.fold(model)(nel => model.setData(decl, nel.list: _*))
      }
    }

    def eval[A, B](expr: A)(implicit Eval: amphip.eval.Eval[A, B]): B = amphip.eval(expr, m.data)

    def solve = solver.GLPSOL.solve(m)

  }

}