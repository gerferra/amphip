package amphip.sem.mathprog

import scalaz.syntax.show._

import amphip.model.ast._
import amphip.model.show._

object genModel {
  
  def apply(model: Model): String = model.shows + "\n"

}