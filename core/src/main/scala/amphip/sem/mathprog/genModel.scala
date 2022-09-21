package amphip.sem.mathprog

import cats.syntax.show._

import amphip.model.ast._
import amphip.model.show._

object genModel {
  
  def apply(model: Model): String = model.show + "\n"

}