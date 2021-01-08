package amphip.data

import scala.language.implicitConversions

//import scalaz.std.list.listSyntax._
import cats.syntax.list._

import amphip.model.ast._
import amphip.model.dsl._
import amphip.data.ops._
import amphip.solver

object syntax extends AllSyntax

trait AllSyntax {

  implicit def ModelAsModelWithData(m: Model): ModelWithData = ModelWithData(m, ModelData())

  implicit class ModelWithDataSyntax[M](m: M)(implicit conv: M => ModelWithData) {
    private def update(model: Model): ModelWithData = m.copy(model = model)

    def relax(xvar: VarStat): ModelWithData = update(m.model.relax(xvar))
    def relax: ModelWithData = update(m.model.relax)

    def replace[A](target: A, replacement: A): ModelWithData = update(m.model.replace(target, replacement))

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
    def ++:(stats: List[Stat]): ModelWithData = update(stats ++: m.model)
    def :++(stats: List[Stat]): ModelWithData = update(m.model :++ stats)

    ////

    def declData[A,B](decl: A, values: B*)(implicit DataOp: DataOp[A, B]): ModelWithData = {
      val newData = DataOp.data(decl, values.toList)(m.data)
      m.copy(data = m.data + newData)
    }
    def declDataList[A,B](list: List[(A, List[B])])(implicit DataOp: DataOp[A, B]): ModelWithData = {
      list.foldLeft(m: ModelWithData) {
        case (model, (decl, data)) => data.toNel.fold(model)(nel => model.declData(decl, nel.toList: _*))
      }
    }

    def paramData[B](decl: ParamStat, values: B*)(implicit DataOp: DataOp[ParamStat, B]) = declData(decl, values: _*)
    def setData  [B](decl: SetStat  , values: B*)(implicit DataOp: DataOp[SetStat  , B]) = declData(decl, values: _*)
    
    def paramDataList[B](list: List[(ParamStat, List[B])])(implicit DataOp: DataOp[ParamStat, B]) = declDataList(list)
    def setDataList  [B](list: List[(SetStat  , List[B])])(implicit DataOp: DataOp[SetStat  , B]) = declDataList(list)

    def eval[A, B](expr: A)(implicit Eval: amphip.data.eval.Eval[A, B]): B = amphip.data.eval(expr, m.data)

    def solve = solver.GLPSOL.solve(m)

  }

}