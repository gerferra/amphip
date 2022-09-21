package amphip.stoch

import cats.Show
import cats.syntax.show._

import amphip.data.show._
import amphip.stoch.dsl._

object show extends ShowInstances

trait ShowInstances {

  implicit val StochModelShow: Show[StochModel] = Show.show {
    case x => x.mip.show
  }

}