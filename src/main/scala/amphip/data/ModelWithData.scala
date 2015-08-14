package amphip.data

import scala.language.implicitConversions

import scalaz.std.list.listSyntax._
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
    sets: SetStatData = LinkedMap.empty) {

  def plusParam(k: DataKey, d: SimpleData): ModelData = copy(params = params + (k -> d))
  def plusSet(k: DataKey, d: List[SetData]): ModelData = copy(sets = sets + (k -> d))

  def plusParams(d: ParamStatData): ModelData = copy(params = params ++ d)
  def plusSets(d: SetStatData): ModelData = copy(sets = sets ++ d)

  def params(d: ParamStatData): ModelData = copy(params = d)
  def sets(d: SetStatData): ModelData = copy(sets = d)

  def +(m: ModelData): ModelData = ModelData(
    params = params ++ m.params,
    sets = sets ++ m.sets)
}

object ModelData {
  type SetStatData = LinkedMap[DataKey, List[SetData]]
  type ParamStatData = LinkedMap[DataKey, SimpleData]
}

case class DataKey(name: String, subscript: List[SimpleData] = Nil) {
  override def toString = {
    val subsTxt = subscript.toNel.fold("") { nel => "[" + nel.list.map(_.shows).mkString(",") + "]" }
    s"$name$subsTxt"
  }
}

object DataKey {
  def apply(name: String, subscript: SimpleData*): DataKey = apply(name, subscript.toList)
}

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

sealed trait SetData
case class SetVal(x: SimpleData) extends SetData
case class SetTuple(values: List[SimpleData]) extends SetData
