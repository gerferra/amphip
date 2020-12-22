package amphip.data

import scalaz._, Scalaz._

import amphip.model.show._

object show extends ShowInstances

trait ShowInstances {

  implicit val ModelWithDataShow: Show[ModelWithData] = Show.show {
    case x => x.model.show
  }
  
  implicit val DataKeyShow: Show[DataKey] = Show.showFromToString[DataKey]

  implicit val SetDataShow: Show[SetData] = Show.show {
    case SetData(x :: Nil) => SimpleDataShow.show(x)
    case SetData(xs)       => xs.map(_.shows).mkString("(", ", ", ")")
  }

  implicit val SimpleDataShow: Show[SimpleData] = Show.show {
    case SimpleNum(x) => x.show
    case SimpleStr(x) => x.show
  }
  
  implicit val ListSetDataShow: Show[List[SetData]] = Show.show { list =>
    list.map(_.shows).mkString(" ")
  }
}
