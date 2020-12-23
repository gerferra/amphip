package amphip.data

import scala.language.implicitConversions

import spire.math._
import spire.implicits._

import amphip.base._
import amphip.model.ast._
import amphip.data.ops._
import amphip.data.ModelData._

object instances extends AllInstances

trait AllInstances extends SimpleDataInstances
  with ParamDataInstances
  with SetDataInstances

trait SimpleDataInstances {

  implicit def NumericAsSimpleData[A: Numeric](a: A): SimpleData = SimpleNum(a.toBigDecimal)

  implicit def StringAsSimpleData(x: String): SimpleData = SimpleStr(x)

  implicit def Tuple2Tuple2AsTupleSimpleData[A, B, C](t: ((A, B), C))(
    implicit convA: A => SimpleData,
             convB: B => SimpleData,
             convC: C => SimpleData): (List[SimpleData], SimpleData) = t match {
               case ((a, b), c) => List(convA(a), convB(b)) -> convC(c)
             }

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

  implicit def ParamStatExtensiveDataOp[A, B](implicit convA: A => ParamStat,
                                                       convB: B => SimpleData): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => values match {
          case Nil => ModelData()
          case _   => ModelData(params = LinkedMap(key(decl.name) -> values.head))
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

  implicit def ParamStatExtensiveDataListOp[A, B](implicit convA: A => ParamStat,
                                                           convB: B => List[SimpleData]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      ParamStatExtensiveDataOp[ParamStat, SimpleData].data(decl, values.flatten)
  }

  implicit def ParamStatIndexedDataOp[A, B](implicit convA: A => ParamStat,
                                                     convB: B => (List[SimpleData], SimpleData)): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => 
          // allows empty Nil in the key part of the head of the values
          values.headOption.map(convB) match {
            case Some((Nil, head)) => ModelData(params = LinkedMap(key(decl.name) -> head))
            case _ => ModelData()
          }
        case Some(indexing) =>
          val evIndSet = eval(indexing).map(_.values.toList).toSet
          val valuesFilter = values.filter(x => evIndSet(x._1)).map(x => x._1 -> x._2)
          val pairs = valuesFilter.map { case (subscript, value) => key(decl.name, subscript) -> value }

          ModelData(params = LinkedMap(pairs: _*))
      }
    }
  }
  
  implicit def ParamStatIndexedDataOp1[A, B](implicit convA: A => ParamStat,
                                                      convB: B => (SimpleData, SimpleData)): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      val newValues = values.map { x => 
        val (k,v) = convB(x)
        List(k) -> v 
      }
      ParamStatIndexedDataOp[ParamStat, (List[SimpleData], SimpleData)].data(decl, newValues)
    }
  }
  
  implicit def ParamStatIndexedDataListOp[A, B](implicit convA: A => ParamStat,
                                                         convB: B => List[(List[SimpleData], SimpleData)]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      ParamStatIndexedDataOp[ParamStat, (List[SimpleData], SimpleData)].data(decl, values.flatten)
  }

}

trait SetDataInstances {

  private[this] val key = DataKey

  implicit def SimpleDataAsSetTuple[A](x: A)(implicit conv: A => SimpleData): SetTuple = SetTuple(List(x))

  implicit def Tuple2SimpleDataAsSetTuple[A, B](t: (A, B))(
    implicit  convA: A => SimpleData,
              convB: B => SimpleData): SetTuple = SetTuple(List(t._1, t._2))

  implicit def IterableSimpleDataAsSetTuple[A](x: Iterable[A])(
    implicit conv: A => SimpleData): SetTuple = SetTuple(x.map(conv).toList)

  implicit def IterableSetTupleAsSetData[A](x: Iterable[A])(
    implicit conv: A => SetTuple): SetData = x.map(conv).toList

  implicit def TupleAsTupleSetData[A, B](t: (A, B))(
    implicit  convA: A => List[SimpleData],
              convB: B => SetData): (List[SimpleData], SetData) = convA(t._1) -> convB(t._2)
    
  implicit def TupleAsTupleSetData1[A, B](t: (A, B))(
    implicit  convA: A => SimpleData,
              convB: B => SetData): (List[SimpleData], SetData) = List(convA(t._1)) -> convB(t._2)

  implicit def IterableAsListTupleSetData[A](xs: Iterable[A])(
    implicit conv: A => (List[SimpleData], SetData)): List[(List[SimpleData], SetData)] =
    xs.map(TupleAsTupleSetData(_)).toList

  implicit def SetStatExtensiveDataOp[A, B](
    implicit  convA: A => SetStat,
              convB: B => SetData): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => 
          values match {
            case Nil => ModelData()
            case _ => ModelData(sets = LinkedMap(key(decl.name) -> values.head))
          }
        case Some(indexing) =>
          val pairs =
            for {
              (localData, localValues) <- eval(indexing).zip(values)
            } yield {
              val subscript = localData.values.toList
              val k = key(decl.name, subscript)
              k -> localValues.map(x => x: SetTuple)
            }

          ModelData(sets = LinkedMap(pairs: _*))
      }
    }
  }

  implicit def SetStatExtensiveDataListOp[A, B](
    implicit  convA: A => SetStat,
              convB: B => List[SetData]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      SetStatExtensiveDataOp[SetStat, SetData].data(decl, values.flatten)
  }

  implicit def SetStatIndexedDataOp[A, B](implicit convA: A => SetStat,
                                                   convB: B => (List[SimpleData], SetData)): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      decl.domain match {
        case None => 
          // allows empty Nil in the key part of the head of the values
          values.headOption.map(convB) match {
            case Some((Nil, head)) => ModelData(sets = LinkedMap(key(decl.name) -> head))
            case _ => ModelData()
          }
        case Some(indexing) =>
          val evIndSet = eval(indexing).map(_.values.toList).toSet
          val valuesFilter = values.filter(x => evIndSet(x._1)).map(x => x._1 -> x._2)
          val pairs = valuesFilter.map { case (subscript, values) => key(decl.name, subscript) -> values }

          ModelData(sets = LinkedMap(pairs: _*))
      }
    }
  }
  
  implicit def SetStatIndexedDataOp1[A, B](implicit convA: A => SetStat,
                                                    convB: B => (SimpleData, SetData)): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData = {
      val newValues = values.map { x => 
        val (k,v) = convB(x)
        List(k) -> v 
      }
      SetStatIndexedDataOp[SetStat, (List[SimpleData], SetData)].data(decl, newValues)
    }
  }

  implicit def SetStatIndexedDataListOp[A, B](
    implicit  convA: A => SetStat,
              convB: B => List[(List[SimpleData], SetData)]): DataOp[A, B] = new DataOp[A, B] {
    def data(decl: A, values: List[B])(implicit modelData: ModelData): ModelData =
      SetStatIndexedDataOp[SetStat, (List[SimpleData], SetData)].data(decl, values.flatten)
  }
}

