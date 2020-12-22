package amphip.data

//import scalaz.std.list.listSyntax._
import cats.syntax.list._
import scalaz.syntax.show._

import spire.algebra._
import spire.implicits._

import amphip.base._
import amphip.model.ast._
import amphip.data.show._
import amphip.data.ModelData._

case class ModelWithData(model: Model, data: ModelData)

case class ModelData(
    params: ParamStatData = LinkedMap.empty,
    sets  : SetStatData   = LinkedMap.empty,
    setsExpansion  : Expansion[SetStat]   = LinkedMap.empty, 
    paramsExpansion: Expansion[ParamStat] = LinkedMap.empty,
    varsExpansion  : Expansion[VarStat]   = LinkedMap.empty) {

  def plusParam(k: DataKey, d: SimpleData)   : ModelData = copy(params = params + (k -> d))
  def plusSet  (k: DataKey, d: SetData): ModelData = copy(sets   = sets   + (k -> d))

  def plusParams(d: ParamStatData): ModelData = copy(params = params ++ d)
  def plusSets  (d: SetStatData)  : ModelData = copy(sets   = sets   ++ d)
  
  def plusParamsExpansion(d: Expansion[ParamStat]): ModelData = copy(paramsExpansion = paramsExpansion ++ d)
  def plusSetsExpansion  (d: Expansion[SetStat])  : ModelData = copy(setsExpansion   = setsExpansion ++ d)
  def plusVarsExpansion  (d: Expansion[VarStat])  : ModelData = copy(varsExpansion   = varsExpansion ++ d)

  def params(d: ParamStatData): ModelData = copy(params = d)
  def sets  (d: SetStatData)  : ModelData = copy(sets   = d)

  def +(m: ModelData): ModelData = ModelData(
    params          = params          ++ m.params,
    sets            = sets            ++ m.sets,
    setsExpansion   = setsExpansion   ++ m.setsExpansion,
    paramsExpansion = paramsExpansion ++ m.paramsExpansion,
    varsExpansion   = varsExpansion   ++ m.varsExpansion)

  def filterParams(pred: DataKey => Boolean): ModelData =
    params(params.filter { case (key, _) => pred(key) })

  def filterSets(pred: DataKey => Boolean): ModelData =
    sets(sets.filter { case (key, _) => pred(key)})
}

object ModelData {
  // XXX define `toString'?
  sealed trait SimpleData
  case class SimpleNum(num: BigDecimal) extends SimpleData
  case class SimpleStr(str: String) extends SimpleData

  object SimpleData {
    implicit class SimpleDataOps(val sd: SimpleData) extends AnyVal {
      def fold[A](ifNum: BigDecimal => A, ifStr: String => A): A = sd match {
        case SimpleNum(num) => ifNum(num)
        case SimpleStr(str) => ifStr(str)
      }

      def numOr(default: => BigDecimal): BigDecimal = fold(identity    , _ => default)
      def strOr(default: => String    ): String     = fold(_ => default, identity)
    }

    implicit val SimpleDataOrder: Order[SimpleData] = new Order[SimpleData] {
      def compare(x: SimpleData, y: SimpleData): Int =
        x -> y match {
          case (SimpleNum(x), SimpleNum(y)) => Order[BigDecimal].compare(x, y)
          case (x, y) =>
            Order[String].compare(
              x.fold(_.toString, identity),
              y.fold(_.toString, identity))
        }
    }
  }

  type SetData = List[SetTuple]
  // wraps a list representing a tuple
  case class SetTuple(values: List[SimpleData])

  case class DataKey(name: String, subscript: List[SimpleData] = Nil) {
    override def toString = {
      val subsTxt = subscript.toNel.fold("") { nel => "[" + nel.toList.map(_.shows).mkString(",") + "]" }
      s"$name$subsTxt"
    }
  }
  object DataKey {
    def apply(name: String, subscript: SimpleData*): DataKey = apply(name, subscript.toList)
  }


  type SetStatData   = LinkedMap[DataKey, SetData]
  type ParamStatData = LinkedMap[DataKey, SimpleData]

  type IndexingData = List[LinkedMap[DataKey, SimpleData]]
  type Expansion[A] = LinkedMap[DataKey, A]
  type LazyExpansion[A] = LinkedMap[DataKey, () => A]
}



