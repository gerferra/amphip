package amphip.data

import scalaz._, Scalaz._

import amphip.model.show._
import amphip.data.ModelData._

object show extends ShowInstances

trait ShowInstances {

  implicit val ModelWithDataShow: Show[ModelWithData] = Show.show {
    case x => x.model.show
  }
  
  implicit val DataKeyShow: Show[DataKey] = Show.showFromToString[DataKey]

  implicit val SetDataShow: Show[SetTuple] = Show.show {
    case SetTuple(x :: Nil) => SimpleDataShow.show(x)
    case SetTuple(xs)       => xs.map(_.shows).mkString("(", ", ", ")")
  }

  implicit val SimpleDataShow: Show[SimpleData] = Show.show {
    case SimpleNum(x) => x.show
    case SimpleStr(x) => x.show
  }
  
  implicit val ListSetDataShow: Show[SetData] = Show.show { list =>
    list.map(_.shows).mkString(" ")
  }
}
