package amphip.model

import org.junit.Assert._
import org.junit.Test

class TestReplace {
  import amphip.dsl._
  import amphip.model.ast.{ParamStat, Model, ParamAssign, NumLit}
  import amphip.data.{SimpleData, DataKey, SimpleNum}

  @Test
  def testLazyReplace(): Unit = {
    val s = dummy
    val S = set := List(1, 2)
    lazy val p: ParamStat = param(s in S) := cond(s === 1) { 1 } { p(s - 1) * 2 }
    val m = model(maximize { sum(s in S)(p(s)) })

    checkParams(m, p, SimpleNum(2))

    lazy val p2: ParamStat = param("p", s in S) := cond(s === 1) { 1 } { p2(s - 1) * 4 }
    val m2 = m.replace(p, p2)

    checkParams(m2, p2, SimpleNum(4))
  }

  def checkParams(m: Model, p: ParamStat, evaluation: SimpleData): Unit = {
    // ParamRefEval
    assertEquals(evaluation, m.eval(p(2)))
    
    // ParamStatEval
    val paramMap = m.eval(p)

    val param2 = paramMap(DataKey("p", SimpleNum(2)))
    val param2Assing =
      param2.atts
        .collect({
          case ParamAssign(NumLit(num)) => SimpleNum(num)
        })
        .headOption
    
    assertEquals(Some(evaluation), param2Assing)
  }
}

