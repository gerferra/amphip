package amphip.data

import org.junit.Assert._
import org.junit.Test

class TestLazyEval {
  import amphip.model.ast._

  /* TODO
    evaluate changing ParamStat and ParamRef and use a String withe the name of 
    the statement instead of the actual statement. Forces to always mantain the 
    expansions in the ModelData...
  */

  @Test
  def testLazyEvalUsingAST(): Unit = {

    val s = DummyIndDecl("s")
    val S = SetStat(
      "S",
      atts = List(SetAssign(SetLit(List(NumLit(1)), List(NumLit(2)))))
    )

    lazy val p: ParamStat = ParamStat(
      "p",
      domain =
        Option(IndExpr(List(IndEntry(indices = List(s), set = SetRef(S))))),
      atts = List(
        ParamAssign(
          CondNumExpr(
            Eq(DummyIndRef(s), NumLit(1)),
            NumLit(1),
            Option(NumMult(ParamRef(p, List(NumSub(DummyIndRef(s), NumLit(1)))), NumLit(2)))
          )
        )
      )
    )

    checkParams(p)
  }

  @Test
  def testLazyEvalUsingDSL(): Unit = {
    import amphip.dsl._

    val s = dummy
    val S = set := List(1, 2)
    lazy val p: ParamStat = param(s in S) := cond(s === 1) { 1 } { p(s - 1) * 2 }

    checkParams(p)
  }

  def checkParams(p: ParamStat): Unit = {
    // ParamRefEval
    {
      import amphip.dsl._
      assertEquals(SimpleNum(2), eval(p(2), ModelData()))
    }
    
    // ParamStatEval
    val paramMap = eval(p, ModelData())
    val param2 = paramMap(DataKey("p", SimpleNum(2)))
    val param2Assing =
      param2.atts
        .collect({
          case ParamAssign(NumLit(num)) => SimpleNum(num)
        })
        .headOption
    
    assertEquals(Some(SimpleNum(2)), param2Assing)
  }
}

