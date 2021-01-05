
/*
 * "Financial planning problem" based on 
 * http://dx.doi.org/10.1007/978-1-4614-0237-4.
 */

import spire.implicits._

import amphip.dsl._
import amphip.stoch.{Stage, BasicScenario}

object finance {

  object base {
    val t = dummy
    val T = set
    val H = param := max(t in T)(t)

    val i = dummy
    val I = set

    val s = dummy
    val S = set

    val G = param
    val b = param
    val xi = param(ind(t in T, S, I) | t > 1)
    val q = param
    val r = param

    val pi = param(S)

    val x = xvar(ind(t in T, S, I) | t < H) >= 0
    val y = xvar(S) >= 0
    val w = xvar(S) >= 0

    val utility = 
      maximize { 
        sum(s in S) { pi(s) * (q * y(s) - r * w(s)) } 
      }

    val budget = 
      st(s in S) {
        sum(i in I) { x(1,s,i) } === b
      }

    val balance =
      st(t in (2 to H-1), s in S) {
        sum(i in I) { xi(t,s,i) * x(t-1,s,i) } === 
        sum(i in I) { x(t,s,i) }
      }

    val goal =
      st(s in S) {
        sum(i in I) { xi(H,s,i) * x(H-1,s,i) } - y(s) + w(s) === G
      }

    val stochModel = 
      model(utility, budget, balance, goal).stochastic(T, S, pi)

    val (stock, bonds) = ("stock", "bonds")
    val stochModelDetData =
      stochModel
        .setData(I, List(stock, bonds))
        .paramData(G, 80000)
        .paramData(b, 55000)
        .paramData(q, 1)
        .paramData(r, 4)

    val (t1, t2, t3, t4) = 
      (Stage("1"), Stage("2"), Stage("3"), Stage("4"))
    val (init, high, low) = 
      (BasicScenario("init"), BasicScenario("high"), BasicScenario("low"))

    val stochModelStages = stochModelDetData.stochStages(t1, t2, t3, t4)
  }

  object defaultAPI {
    import base._ 

    val stochModelBasicScenarios =
      stochModelStages
        .stochBasicScenarios(t1, init -> r"1")
        .stochBasicScenarios(t2, high -> r"1/2", low -> r"1/2")
        .stochBasicScenarios(t3, high -> r"1/2", low -> r"1/2")
        .stochBasicScenarios(t4, high -> r"1/2", low -> r"1/2")

    val stochModelBasicData =
      stochModelBasicScenarios
        .stochBasicData(xi, t2, high, stock -> 1.25, bonds -> 1.14)
        .stochBasicData(xi, t2, low , stock -> 1.06, bonds -> 1.12)
        .stochBasicData(xi, t3, high, stock -> 1.25, bonds -> 1.14)
        .stochBasicData(xi, t3, low , stock -> 1.06, bonds -> 1.12)
        .stochBasicData(xi, t4, high, stock -> 1.25, bonds -> 1.14)
        .stochBasicData(xi, t4, low , stock -> 1.06, bonds -> 1.12)

    val mipEquiv = stochModelBasicData.mip

    val modelStr = amphip.sem.mathprog.genModel(mipEquiv.model)
    val dataStr = amphip.sem.mathprog.genData(mipEquiv.data)

    val (sout, out) = mipEquiv.solve
  }

  object extendedAPI {
    import base._, aux._ 
    
    val stochModelBasicScenarios = 
      stochPerfectTree(stochModelStages, init, List(high, low))

    val stochModelBasicData =
      stochStagesData(stochModelBasicScenarios, xi, List(t2,t3,t4),
        high -> List(stock -> 1.25, bonds -> 1.14),
        low  -> List(stock -> 1.06, bonds -> 1.12)
      )

    val mipEquiv = stochModelBasicData.mip

    val modelStr = amphip.sem.mathprog.genModel(mipEquiv.model)
    val dataStr = amphip.sem.mathprog.genData(mipEquiv.data)

    val (sout, out) = mipEquiv.solve
  }

  object aux {
    import cats.syntax.list._
    import amphip.model.ast.ParamStat
    import amphip.data.ops.DataOp
    import amphip.stoch.StochModel

    def stochPerfectTree(
      m           : StochModel, 
      init        : BasicScenario, 
      alternatives: Iterable[BasicScenario]): StochModel = {

      m.stochStages.toNel.fold(m) { nel =>
        val m1 = m.stochBasicScenarios(nel.head, init -> r"1")

        val den     = alternatives.size
        val prob    = r"1" / den
        val altProb = alternatives.map(_ -> prob)

        nel.tail.foldLeft(m1) { (model, t) =>
          model.stochBasicScenarios(t, altProb.toSeq: _*)
        }
      }
    }

    def stochStagesData[B](
      m         : StochModel, 
      p         : ParamStat, 
      stages    : List[Stage],
      bsDataList: (BasicScenario, B)*)
      (implicit ev: DataOp[ParamStat, B]): StochModel = {

      stages.foldLeft(m) { (model0, t) =>
        bsDataList.foldLeft(model0) { (model1, bsData) =>
          val (bs, data) = bsData
          model1.stochBasicData(p, t, bs, data)
        }
      }
    }
    
  }
}
