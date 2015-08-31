package amphip.data.dsl

import scala.language.implicitConversions

import spire.math._
import spire.implicits._

import amphip.base._
import amphip.model.ast._
import amphip.data._
import amphip.data.dsl.ops._
import amphip.eval

object instances extends AllInstances

trait AllInstances extends SimpleDataInstances
  with ParamDataInstances
  with SetDataInstances

trait SimpleDataInstances {

  implicit def NumericAsSimpleData[A: Numeric](a: A): SimpleData = SimpleNum(a.toBigDecimal)

  implicit def StringAsSimpleData(x: String): SimpleData = SimpleStr(x)

  implicit def Tuple2AsTupleSimpleData[A, B](t: (A, B))(
    implicit convA: A => List[SimpleData],
    convB: B => SimpleData): (List[SimpleData], SimpleData) = convA(t._1) -> convB(t._2)
    
  implicit def Tuple2AsTupleSimpleData1[A, B](t: (A, B))(
    implicit convA: A => SimpleData,
    convB: B => SimpleData): (List[SimpleData], SimpleData) = List(convA(t._1)) -> convB(t._2)

  implicit def IterableAsListSimpleData[A](xs: Iterable[A])(
    implicit conv: A => SimpleData): List[SimpleData] = xs.map(conv).toList

  implicit def IterableAsListTupleSimpleData[A](xs: Iterable[A])(
    implicit conv: A => (List[SimpleData], SimpleData)): List[(List[SimpleData], SimpleData)] =
    xs.map(Tuple2AsTupleSimpleData(_)).toList
}

trait ParamDataInstances {

  private[this] val key = DataKey

  implicit def ParamStatExtensiveDataOp[A, B](
    implicit convA: A => ParamStat,
    convB: B => SimpleData): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => values match {
          case Nil => ModelData()
          case List(v) => ModelData(params = LinkedMap(key(decl.name) -> v))
        }
        case Some(indexing) =>
          val pairs =
            for {
              (localData, localValue) <- eval(indexing).zip(values)
            } yield {
              val subscript = localData.values.toList
              val k = key(decl.name, subscript)
              (k, localValue: SimpleData)
            }

          ModelData(params = LinkedMap(pairs: _*))
      }
    }
  }

  implicit def ParamStatExtensiveDataListOp[A, B](
    implicit convA: A => ParamStat,
    convB: B => List[SimpleData]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      ParamStatExtensiveDataOp[ParamStat, SimpleData].data(decl, values.flatten)
  }

  implicit def ParamStatIndexedDataOp[A, B](
    implicit convA: A => ParamStat,
    convB: B => (List[SimpleData], SimpleData)): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => ModelData()
        case Some(indexing) =>
          val evInd = eval(indexing).map(_.values.toList)
          val valuesFilter = values.filter(x => evInd.contains(x._1)).map(x => x._1 -> x._2)
          val pairs = valuesFilter.map { case (subscript, value) => key(decl.name, subscript) -> value }

          ModelData(params = LinkedMap(pairs: _*))
      }
    }
  }
  
  implicit def ParamStatIndexedDataOp1[A, B](
    implicit convA: A => ParamStat,
    convB: B => (SimpleData, SimpleData)): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => ModelData()
        case Some(indexing) =>
          val evInd = eval(indexing).map(_.values.toList)
          val valuesFilter = values.filter(x => evInd.contains(List(x._1))).map(x => List(x._1) -> x._2)
          val pairs = valuesFilter.map { case (subscript, value) => key(decl.name, subscript) -> value }

          ModelData(params = LinkedMap(pairs: _*))
      }
    }
  }
  
  implicit def ParamStatIndexedDataListOp[A, B](
    implicit convA: A => ParamStat,
    convB: B => List[(List[SimpleData], SimpleData)]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      ParamStatIndexedDataOp[ParamStat, (List[SimpleData], SimpleData)].data(decl, values.flatten)
  }

}

trait SetDataInstances {

  private[this] val key = DataKey

  implicit def SimpleDataAsSetData[A](x: A)(implicit conv: A => SimpleData): SetData = SetVal(x)

  implicit def IterableSimpleDataAsSetData[A](x: Iterable[A])(
    implicit conv: A => SimpleData): SetData = SetTuple(x.map(conv).toList)

  implicit def IterableSetDataAsListSetData[A](x: Iterable[A])(
    implicit conv: A => SetData): List[SetData] = x.map(conv).toList

  implicit def TupleAsTupleListSetData[A, B](t: (A, B))(
    implicit convA: A => List[SimpleData],
    convB: B => List[SetData]): (List[SimpleData], List[SetData]) = convA(t._1) -> convB(t._2)
    
  implicit def TupleAsTupleListSetData1[A, B](t: (A, B))(
    implicit convA: A => SimpleData,
    convB: B => List[SetData]): (List[SimpleData], List[SetData]) = List(convA(t._1)) -> convB(t._2)

  implicit def IterableAsListTupleListSetData[A](xs: Iterable[A])(
    implicit conv: A => (List[SimpleData], List[SetData])): List[(List[SimpleData], List[SetData])] =
    xs.map(TupleAsTupleListSetData(_)).toList

  implicit def SetStatExtensiveDataOp[A, B](
    implicit convA: A => SetStat,
    convB: B => List[SetData]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => values match {
          case Nil => ModelData()
          case List(v) => ModelData(sets = LinkedMap(key(decl.name) -> v))
        }
        case Some(indexing) =>
          val pairs =
            for {
              (localData, localValues) <- eval(indexing).zip(values)
            } yield {
              val subscript = localData.values.toList
              val k = key(decl.name, subscript)
              k -> localValues.map(x => x: SetData)
            }

          ModelData(sets = LinkedMap(pairs: _*))
      }
    }
  }

  implicit def SetStatExtensiveDataListOp[A, B](
    implicit convA: A => SetStat,
    convB: B => List[List[SetData]]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      SetStatExtensiveDataOp[SetStat, List[SetData]].data(decl, values.flatten)
  }

  implicit def SetStatIndexedDataOp[A, B](
    implicit convA: A => SetStat,
    convB: B => (List[SimpleData], List[SetData])): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => ModelData()
        case Some(indexing) =>
          val evInd = eval(indexing).map(_.values.toList)
          val valuesFilter = values.filter(x => evInd.contains(x._1)).map(x => x._1 -> x._2)
          val pairs = valuesFilter.map { case (subscript, values) => key(decl.name, subscript) -> values }

          ModelData(sets = LinkedMap(pairs: _*))
      }
    }
  }
  
  implicit def SetStatIndexedDataOp1[A, B](
    implicit convA: A => SetStat,
    convB: B => (SimpleData, List[SetData])): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => ModelData()
        case Some(indexing) =>
          val evInd = eval(indexing).map(_.values.toList)
          val valuesFilter = values.filter(x => evInd.contains(List(x._1))).map(x => List(x._1) -> x._2)
          val pairs = valuesFilter.map { case (subscript, values) => key(decl.name, subscript) -> values }

          ModelData(sets = LinkedMap(pairs: _*))
      }
    }
  }

  implicit def SetStatIndexedDataListOp[A, B](
    implicit convA: A => SetStat,
    convB: B => List[(List[SimpleData], List[SetData])]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      SetStatIndexedDataOp[SetStat, (List[SimpleData], List[SetData])].data(decl, values.flatten)
  }
}

