package amphip.solver

import amphip.data._

trait Solver {

  type Ret = (String, String)
  def solve(model: ModelWithData): Ret

}