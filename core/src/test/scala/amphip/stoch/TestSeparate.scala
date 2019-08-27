package amphip.stoch

import org.junit.Assert._
import org.junit.Test

import amphip.dsl._
import amphip.model.syntax.{ model => smodel } // `model' method clashes with package `amphip.model'
import amphip.model.ast._
import amphip.data._

class TestSeparate {

  @Test
  def test(): Unit = {

    val gen = newGen

    val S = set("S")
    val s = dummy("s")
    val s_ = dummy(gen.dummy("s").freshName)
    val p = param("p", S)

    val x = xvar("x") >= 0
    val xSep = xvar("x", s_ in S) >= 0

    val m1 = smodel(minimize("obj") { x }).stochastic(S, p).separate
    val m1Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * xSep(s_) } })

    assertEq("deterministic variables should add an index on `S'", m1Sep, m1)

    val y = xvar("y") >= 0
    val ySep = xvar("y", s_ in S) >= 0

    val m2 = smodel(minimize("obj") { x + y }).stochastic(S, p).separate
    val m2Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * (xSep(s_) + ySep(s_)) } })

    assertEq("deterministic variables should add an index on `S'", m2Sep, m2)

    // TODO cleanup. some test are obsolete because of the wrong assumption that VarAtt could have LinExpr expressions

    val z = xvar("z") >= 0
    val zC = st("zC") { z >= x }
    val zSep = xvar("z", s_ in S) >= 0
    val zSepC = st("zC", s_ in S) { zSep(s_) >= xSep(s_) }

    val m3 = smodel(minimize("obj") { x + z }, zC).stochastic(S, p).separate
    val m3Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * (xSep(s_) + zSep(s_)) } }, zSepC)

    assertEq("separation of deterministic variable should be handled in dependant deterministic variables", m3Sep, m3)

    val z1 = xvar("z1") >= 0
    val z1C = st("z1C") { z1 >= x }
    val z1Sep = xvar("z1", s_ in S) >= 0
    val z1SepC = st("z1C", s_ in S) { z1Sep(s_) >= xSep(s_) }

    val m4 = smodel(minimize("obj") { x + z + z1 }, zC, z1C).stochastic(S, p).separate
    val m4Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * (xSep(s_) + zSep(s_) + z1Sep(s_)) } }, zSepC, z1SepC)

    assertEq("separation of deterministic variable should be handled in dependant deterministic variables", m4Sep, m4)

    val w = xvar("w") >= 0
    val wC = st("wC") { w >= y }
    val wSep = xvar("w", s_ in S) >= 0
    val wSepC = st("wC", s_ in S) { wSep(s_) >= ySep(s_) }

    val m5 = smodel(minimize("obj") { x + y + z + w }, zC, wC).stochastic(S, p).separate
    val m5Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * (xSep(s_) + ySep(s_) + zSep(s_) + wSep(s_)) } }, zSepC, wSepC)

    assertEq("separation of deterministic variable should be handled in dependant deterministic variables", m5Sep, m5)

    val yStoch = xvar("y", S) >= 0

    val m6 = smodel(minimize("obj") { sum(s in S) { p(s) * yStoch(s) } }).stochastic(S, p).separate
    val m6Sep = smodel(minimize("obj") { sum(s in S) { p(s) * yStoch(s) } })

    assertEq("stochastic variables should be left untouched", m6Sep, m6)

    val zStoch = xvar("z", S) >= 0

    val m7 = smodel(minimize("obj") { sum(s in S) { p(s) * (yStoch(s) + zStoch(s)) } }).stochastic(S, p).separate
    val m7Sep = smodel(minimize("obj") { sum(s in S) { p(s) * (yStoch(s) + zStoch(s)) } })

    assertEq("stochastic variables should be left untouched", m7Sep, m7)

    val z1Stoch = xvar("z", S) >= 0
    val z1StochC = st("zC", s in S) { z1Stoch(s) >= x }
    val z1StochSepC = st("zC", s in S) { z1Stoch(s) >= xSep(s) }

    val m8 = smodel(minimize("obj") { sum(s in S) { p(s) * z1Stoch(s) } }, z1StochC).stochastic(S, p).separate
    val m8Sep = smodel(minimize("obj") { sum(s in S) { p(s) * z1Stoch(s) } }, z1StochSepC)

    assertEq("separation of deterministic variable should be handled in dependant stochastic variables", m8Sep, m8)

    val m9 = smodel(minimize("obj") { sum(s in S) { p(s) * (yStoch(s) + z1Stoch(s)) } }, z1StochC).stochastic(S, p).separate
    val m9Sep = smodel(minimize("obj") { sum(s in S) { p(s) * (yStoch(s) + z1Stoch(s)) } }, z1StochSepC)

    assertEq("separation of deterministic variable should be handled in dependant stochastic variables", m9Sep, m9)

    val y1Stoch = xvar("y", S) >= 0
    val y1StochC = st("yC", s in S) { y1Stoch(s) >= zStoch(s) + x }
    val y1StochSepC = st("yC", s in S) { y1Stoch(s) >= zStoch(s) + xSep(s) }

    val m10 = smodel(minimize("obj") { sum(s in S) { p(s) * (y1Stoch(s) + zStoch(s)) } }, y1StochC).stochastic(S, p).separate
    val m10Sep = smodel(minimize("obj") { sum(s in S) { p(s) * (y1Stoch(s) + zStoch(s)) } }, y1StochSepC)

    assertEq("separation of deterministic variable should be handled in dependant stochastic variables", m10Sep, m10)

    val ctrDet = st("ctr") { x() >= 8 }
    val ctrDetSep = st("ctr", s_ in S) { xSep(s_) >= 8 }

    val m11 = smodel(minimize("obj") { x }, ctrDet).stochastic(S, p).separate
    val m11Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * xSep(s_) } }, ctrDetSep)

    assertEq("separation of deterministic variable should be handled in dependant constraints", m11Sep, m11)

    val ctrStoch = st("ctr", s in S) { yStoch(s) >= x }
    val ctrStochSep = st("ctr", s in S) { yStoch(s) >= xSep(s) }

    val m12 = smodel(minimize("obj") { sum(s in S) { p(s) * yStoch } }, ctrStoch).stochastic(S, p).separate
    val m12Sep = smodel(minimize("obj") { sum(s in S) { p(s) * yStoch } }, ctrStochSep)

    assertEq("separation of deterministic variable should be handled in dependant constraints", m12Sep, m12)

    val m13 = smodel(minimize("obj") { x + y }).stochastic(S, p).separate
    val m13Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * (xSep(s_) + ySep(s_)) } })

    assertEq("separation of deterministic variable should be handled in objective", m13Sep, m13)

    val m14 = smodel(minimize("obj") { sum(s in S) { p(s) * (yStoch(s) + zStoch(s)) } }).stochastic(S, p).separate
    val m14Sep = smodel(minimize("obj") { sum(s in S) { p(s) * (yStoch(s) + zStoch(s)) } })

    assertEq("separation of deterministic variable should be handled in objective", m14Sep, m14)

    val wStoch = xvar("w", S) >= 0

    val m15 = smodel(minimize("obj") { x + y + sum(s in S) { p(s) * (wStoch(s) + zStoch(s)) } }).stochastic(S, p).separate
    val m15Sep = smodel(minimize("obj") { sum(s_ in S) { p(s_) * (xSep(s_) + ySep(s_)) } + sum(s in S) { p(s) * (wStoch(s) + zStoch(s)) } })

    assertEq("separation of deterministic variable should be handled in objective", m15Sep, m15)

  }

  import scalaz._, Scalaz._

  def assertEq(msg: String, mSep: ModelWithData, m: ModelWithData): Unit = {
    val mSepShows = mSep.shows
    val mShows = m.shows
    assertEquals(s"$msg\n$mSepShows\n\n$mShows\n", mSep, m)
  }

}