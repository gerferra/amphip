/*
 * Simplified version of the stochastic model in
 * http://dx.doi.org/10.1590/0101-7438.2019.039.01.0037.
 *
 * Formulation with separated scenarios.
 */

import spire.implicits._

import amphip.dsl._
import amphip.model.ast.ParamStat
import amphip.stoch.{Stage, BasicScenario}

object fuelSupplyMinTSSep {

  import randomData._

  object base {
    // sets (and sets parameters)
    val t = dummy
    val tp = dummy
    val H = param
    val T = set := 1 to H

    val c = dummy
    val A = set
    val P = set
    val C = set := A | P

    val s = dummy
    val S = set

    // parameters (and sets derived from parameters)
    val d = param(T, S)

    val inv0 = param
    val invmin = param
    val invmax = param

    val tau = param(A) in T
    val gamma = param(P) in (0 to H - 1)

    val q = param(C)

    val ca = param(C) 
    val cc = param(A)
    val h = param(T)

    val a = param(t in T) :=
      sum(ind(c in A) | tau(c) === t) { q(c) }

    val pi = param(S)

    // variables
    val inv = xvar(T, S) >= 0

    val v = xvar(ind(c in P, t in T, S) | t <= H - gamma(c)).binary
    val x = xvar(ind(c in A, t in T, S) | t <= tau(c) - 1).binary

    val u = xvar(T, S) >= 0
    val w = xvar(T, S) >= 0

    // objective function
    val cost = minimize {
      sum(t in T, s in S) {
        pi(s) * (
          sum(ind(c in P) | t <= H - gamma(c)) {  ca(c)          * q(c) * v(c,t,s) } +
          sum(ind(c in A) | t <= tau(c) - 1  ) { (cc(c) - ca(c)) * q(c) * x(c,t,s) } +
          h(t) * inv(t,s)
        )
      }
    }

    // constraints
    val balance0 = st(                   s in S) { inv0        + a(1) + u(1,s) === d(1,s) + w(1,s) + inv(1,s) }
    val balance  = st(t in T &~ List(1), s in S) { inv(t-1, s) + a(t) + u(t,s) === d(t,s) + w(t,s) + inv(t,s) };

    val inventory = st(t in T, s in S) { dlte(invmin, inv(t,s), invmax) }

    val singleAcquisition  = st(c in P, s in S) { sum(ind(t in T) | t <= H - gamma(c)) { v(c,t,s) } <= 1 }
    val singleCancellation = st(c in A, s in S) { sum(ind(t in T) | t <= tau(c) - 1  ) { x(c,t,s) } <= 1 }

    val acquiredFuel = st(t in T, s in S) {
      u(t,s) === sum(ind(c in P) | gamma(c) <= t-1) { q(c) * v(c, t-gamma(c), s) }
    }

    val cancelledFuel = st(t in T, s in S) {
      w(t,s) === sum(ind(c in A) | tau(c) === t) { q(c) * sum(ind(tp in T) | tp <= tau(c) - 1) { x(c,tp,s) } }
    }

    // model
    val basicMIP = 
      model(cost, 
        balance0, balance, inventory, singleAcquisition, singleCancellation, acquiredFuel, cancelledFuel)

    // data
    val HData = 3
    val AData = List("A1", "A2")
    val PData = List("P1", "P2", "P3", "P4")

    val basicMIPWData = basicMIP
      .paramData(H, HData)
      .setData(A, AData)
      .setData(P, PData)
      .paramData(inv0, 20)
      .paramData(invmin, 0)
      .paramData(invmax, 80)
      .paramData(tau, "A1" -> 1, "A2" -> 2)
      .paramData(gamma, PData.map(_ -> 1))
      .paramData(q,  uniform(2)( 10,  50)(AData ::: PData))
      .paramData(ca, uniform(3)(150, 250)(AData ::: PData))
      .paramData(cc, uniform(4)( 30,  50)(AData))
      .paramData(h, (1 to HData).map(_ -> 1))
  }

  object NA_byHand {
    import base._ 

    // non-anticipativity by hand-made "adapters"
    val NA_S = set(T)
    val NA_p = param(t in T &~ List(1), NA_S(t)) in NA_S(t-1)

    lazy val NA_pt: ParamStat = 
      param(ind(t in T, s in NA_S(t), tp in T) | tp <= t) in NA_S(tp) :=
        cond (tp === t) { s } { NA_pt(t-1, NA_p(t,s), tp) }

    val NA_ptf = param(s in NA_S(H), t in T) in NA_S(t) := NA_pt(H, s, t)

    val NA_d = param(t in T, NA_S(t))

    val NA_inv = xvar(t in T, NA_S(t)) >= 0
    val NA_inv_ctr = st(t in T, s in S) { inv(t, s) === NA_inv(t, NA_ptf(s,t)) }

    val NA_v = xvar(ind(c in P, t in T, NA_S(t)) | t <= H - gamma(c)).binary
    val NA_v_ctr = st(ind(c in P, t in T, s in S) | t <= H - gamma(c)) { v(c,t,s) === NA_v(c,t,NA_ptf(s,t)) }

    val NA_x = xvar(ind(c in A, t in T, NA_S(t)) | t <= tau(c) - 1).binary
    val NA_x_ctr = st(ind(c in A, t in T, s in S) | t <= tau(c) - 1) { x(c,t,s) === NA_x(c,t,NA_ptf(s,t)) }

    // adaptation of parameters and sets
    val d_ = param("d", t in T, s in S) default NA_d(t, NA_ptf(s,t))
    val S_ = set("S") default NA_S(H)

    // the order of replacements is relevant in this case
    val stochModel = (List(NA_ptf, NA_d) ++: basicMIPWData :++ List(NA_inv_ctr, NA_v_ctr, NA_x_ctr))
      .replace(d, d_) 
      .replace(S, S_)

    val NA_pi = stochModel.param("pi") // `pi` to use versión with new `S`.

    val stochModelWData = stochModel
      .setData(NA_S, 
        1 -> List(1),
        2 -> List(1, 2),
        3 -> List(1, 2, 3, 4))
      .paramData(NA_p,
        List(List(2, 1) -> 1, List(2, 2) -> 1) :::
        List(List(3, 1) -> 1, List(3, 2) -> 1, List(3, 3) -> 2, List(3, 4) -> 2)
      )
      .paramData(NA_pi, beta(1 to 4))
      .paramData(NA_d, 
        uniformDemand(List(List(1, 1))) :::
        uniformDemand(List(List(2, 1), List(2, 2))) :::
        uniformDemand(List(List(3, 1), List(3, 2), List(3, 3), List(3, 4)))
      ) 

    val modelStr = amphip.sem.mathprog.genModel(stochModelWData.model)
    val dataStr = amphip.sem.mathprog.genData(stochModelWData.data)

    val (sout, out) = stochModelWData.solve
  }

  object NA_auto { 
    import base._ 

    val stochModel = basicMIPWData.stochastic(T, S, pi)

    val (t1, t2, t3) = (Stage("1"), Stage("2"), Stage("3"))
    val (init, a, b) = (BasicScenario("init"), BasicScenario("a"), BasicScenario("b"))

    val stochModelStages = stochModel
      .stochStages(t1, t2, t3)

    // basic tree, two alternatives per stage (dummy probabilities)
    val stochModelBasicScenarios = stochModelStages
      .stochBasicScenarios(t1, init -> r"1")
      .stochBasicScenarios(t2, a -> r"1/2", b -> r"1/2")
      .stochBasicScenarios(t3, a -> r"1/2", b -> r"1/2")

    val scenarios = stochModelBasicScenarios.scenarios
    val scenProbs = beta(scenarios).map { case (ss, prob) => ss -> prob.toRational }

    val stochModelFinalProbabilities = 
      stochModelBasicScenarios
        .stochProbabilities(scenProbs)

  }

  object randomData {
    import support._ 

    // data generated from probability distributions

    /**
     * Given a `seed`, returns a function that given a list of elements
     * and a range, returns a list of pairs with the specified elements 
     * matched with uniform random values in the specified range, using 
     * the seed to initialize the random number generator.
     */
    def uniform[A](seed: Long): (Int, Int) => (Iterable[A])  => Iterable[(A, Int)] = {
      val random = new scala.util.Random()
      random.setSeed(seed)
      
      (min: Int, max: Int) =>
        (xs: Iterable[A]) =>
          xs.map(_ -> random.between(min, max))
    }

    val uniformDemand = uniform[List[Int]](1)(10, 50)
      
    def beta[A](xs: Seq[A]): Seq[(A, Double)] = {
      import breeze.stats.distributions.Beta
      
      val betaDist = new Beta(2, 2)
      
      val size = xs.size
      val idx = 1 to size
      val idxNorm = idx.map(_.toDouble / size)
      
      val discretization =
        idxNorm
          .sliding(2)
          .map { case Seq(x, y) => betaDist.probability(x, y) }
          .toSeq
      
      val probs = 
        betaDist.probability(0, idxNorm.head) +: discretization
      
      xs.zip(probs)
    }
  }
    
  object support {
    // TODO update to Scala 2.13
    implicit class RandomOps(x: scala.util.Random) {
      def between(minInclusive: Int, maxExclusive: Int): Int = {
        require(minInclusive < maxExclusive, "Invalid bounds")
        require(
          maxExclusive - minInclusive >= 0,
          "Interval size greater than Int.MaxValue"
        )

        val difference = maxExclusive - minInclusive
        x.nextInt(difference) + minInclusive
      }
    }
  }
}
