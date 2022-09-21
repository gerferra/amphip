package amphip.data

import cats.Show
import cats.instances.all._
import cats.syntax.show._

import amphip.model.show._
import amphip.data.ModelData._

object show extends ShowInstances

trait ShowInstances {

  implicit val ModelWithDataShow: Show[ModelWithData] = Show.show {
    case x => x.model.show
  }
  
  implicit val DataKeyShow: Show[DataKey] = Show.fromToString[DataKey]

  implicit val SetDataShow: Show[SetTuple] = Show.show {
    case SetTuple(x :: Nil) => SimpleDataShow.show(x)
    case SetTuple(xs)       => xs.map(_.show).mkString("(", ", ", ")")
  }

  implicit val SimpleDataShow: Show[SimpleData] = Show.show {
    case SimpleNum(x) => x.show
    case SimpleStr(x) => x.show
  }
  
  implicit val ListSetDataShow: Show[SetData] = Show.show { list =>
    list.map(_.show).mkString(" ")
  }
}
