/*
 * Simplified version of the stochastic model in
 * http://dx.doi.org/10.1590/0101-7438.2019.039.01.0037.
 *
 * Formulation with separated scenarios.
 */

import spire.implicits._
import spire.math.{Rational, Numeric}

import amphip.dsl._
import amphip.model.ast.ParamStat
import amphip.stoch.{Stage, BasicScenario, StochData}, StochData.Scenario

object fuelSupplyMinTSSep {

  import randomData._

  object full {
    // sets (and sets parameters)
    val t  = dummy
    val tp = dummy
    val T = set
    val H = param := max(t in T)(t)

    val c = dummy
    val A = set
    val P = set
    val C = set := A | P

    val s = dummy
    val S = set

    // parameters
    val d    = param(T, S)
    val y0   = param
    val ymin = param
    val ymax = param

    val tau   = param(A) in T
    val gamma = param(P) in (0 to H - 1)
    val q     = param(C)

    val ca = param(C) 
    val cc = param(A)
    val h  = param(T)

    val a = param(t in T) :=
      sum(ind(c in A) | tau(c) === t) { q(c) }

    val pi = param(S)

    // variables
    val y = xvar(T, S) >= 0
    val v = xvar(ind(c in P, t in T, S) | t <= H - gamma(c)).binary
    val x = xvar(ind(c in A, t in T, S) | t <= tau(c) - 1).binary

    val u = xvar(T, S) >= 0
    val w = xvar(T, S) >= 0

    // objective function
    val cost = minimize { 
      sum(t in T, s in S) {
        pi(s) * (
          sum(ind(c in P) | t <= H-gamma(c)) { 
            ca(c) * q(c) * v(c,t,s) 
          } +
          sum(ind(c in A) | t <= tau(c)-1) { 
            (cc(c) - ca(c)) * q(c) * x(c,t,s) 
          } +
          h(t) * y(t,s)
        )
      } 
    }

    // constraints
    val balance0 = st(            s in S         ) { 
      y0        + a(1) + u(1,s) === d(1,s) + w(1,s) + y(1,s) 
    }
    val balance  = st(ind(t in T, s in S) | t > 1) { 
      y(t-1, s) + a(t) + u(t,s) === d(t,s) + w(t,s) + y(t,s) 
    }

    val inventory = st(t in T, s in S) { dlte(ymin, y(t,s), ymax) }

    val singleAcquisition = st(c in P, s in S) { 
      sum(ind(t in T) | t <= H - gamma(c)) { v(c,t,s) } <= 1 
    }
    val singleCancellation = st(c in A, s in S) { 
      sum(ind(t in T) | t <= tau(c) - 1  ) { x(c,t,s) } <= 1 
    }

    val acquiredFuel = st(t in T, s in S) {
      u(t,s) ===  sum(ind(c in P) | gamma(c) <= t-1) { 
                    q(c) * v(c, t-gamma(c), s) 
                  }
    }

    val cancelledFuel = st(t in T, s in S) {
      w(t,s) ===  sum(ind(c in A) | tau(c) === t) { 
                    q(c) * sum(ind(tp in T) | tp <= tau(c)-1) { x(c,tp,s) } 
                  }
    }

    // model
    val stochModel = 
      model(cost, 
        balance0, balance, inventory, 
        singleAcquisition, singleCancellation, 
        acquiredFuel, cancelledFuel
      ).stochastic(T, S, pi)

    // data
    val (t1, t2, t3) = (Stage("1"), Stage("2"), Stage("3"))
    val (init, bsa, bsb) = (BasicScenario("init"), BasicScenario("a"), BasicScenario("b"))

    val stochModelBS = stochModel
      .stochStages(t1, t2, t3)
      .stochBasicScenarios(t1, init -> r"1")
      .stochBasicScenarios(t2, bsa -> r"1/2", bsb -> r"1/2")
      .stochBasicScenarios(t3, bsa -> r"1/2", bsb -> r"1/2")
      
    val stages         = stochModelBS.stages
    val scenarios      = stochModelBS.scenarios
    val finalScenarios = stochModelBS.finalScenarios

    val AData = List("A1", "A2")
    val PData = List("P1", "P2", "P3", "P4")

    val stochModelWData = stochModelBS
      .setData(A, AData)
      .setData(P, PData)
      .paramData(y0  , 20)
      .paramData(ymin,  0)
      .paramData(ymax, 80)
      .paramData(tau, "A1" -> 1, "A2" -> 2)
      .paramData(gamma, PData.map(_ -> 1))
      .paramData(q , uniform(2)( 10,  50)(AData ::: PData))
      .paramData(ca, uniform(3)(150, 250)(AData ::: PData))
      .paramData(cc, uniform(4)( 30,  50)(AData))
      .paramData(h, stages.indices.map(_ + 1 -> 1))
      .stochScenarioData(d, uniformL(1)(10, 50)(scenarios))
      .stochProbabilities(betaR(2, 2)(finalScenarios))

    // mip equivalent
    val mipEquiv = stochModelWData.mip2

    val modelStr = amphip.sem.mathprog.genModel(mipEquiv.model)
    val dataStr  = amphip.sem.mathprog.genData (mipEquiv.data)

    val (sout, out) = mipEquiv.solve
  }

  object value {
    object RP {
      val problem     = full.stochModelWData
      val (sout, out) = problem.solve 
      val value       = 5935 
    }

    object WS {
      val problem     = amphip.stoch.separate(RP.problem)
      val (sout, out) = problem.solve 
      val value       = 2738
    }

    val EVPI = RP.value - WS.value // 3197

    object EV1 {
      val problem     = amphip.stoch.EV(RP.problem, full.t1)
      val (sout, out) = problem.solve
      val value       = 2444
      /*
        v[P1,1,1] = 0
        v[P2,1,1] = 0
        v[P3,1,1] = 0
        v[P4,1,1] = 1
        x[A2,1,1] = 1
      */
    }

    object EEV1 {
      val problem = {
        import full._

        val vbar1 = param(ind(t in T, S, P) | t === 1)
        val xbar1 = param(ind(t in T, S, A) | t === 1)
  
        val fixV1 = st(ind(s in S, c in P) | gamma(c) <= H-1) { v(c,1,s) === vbar1(1,s,c) }
        val fixX1 = st(ind(s in S, c in A) | tau(c)   >= 2)   { x(c,1,s) === xbar1(1,s,c) }

        val List(s1) = RP.problem.stochData.scenariosByStage(t1)
        (RP.problem :++ List(fixV1, fixX1))
          .stochScenarioData(vbar1, s1, 
            "P1" -> 0, 
            "P2" -> 0, 
            "P3" -> 0, 
            "P4" -> 1)
          .stochScenarioData(xbar1, s1, 
            "A2" -> 1)
      }
      val (sout, out) = problem.solve
      val value       = 5935
    }

    object EV2 {
      val problem     = amphip.stoch.EV(EEV1.problem, full.t2)
      val (sout, out) = problem.solve
      val value       = 2461
      /*
        v[P1,2,1] = 0
        v[P1,2,2] = 0
        v[P2,2,1] = 0
        v[P2,2,2] = 0
        v[P3,2,1] = 0
        v[P3,2,2] = 0
        v[P4,2,1] = 0
        v[P4,2,2] = 0
      */
    }

    object EEV2 {
      val problem = {
        import full._

        val vbar2 = param(ind(t in T, S, P) | t === 2)
        val fixV2 = st(ind(s in S, c in P) | gamma(c) <= H-2) { v(c,2,s) === vbar2(2,s,c) }

        val List(s1, s2) = EEV1.problem.stochData.scenariosByStage(t2)
        (EEV1.problem :+ fixV2)
          .stochScenarioData(vbar2, s1, PData.map(_ -> 0))
          .stochScenarioData(vbar2, s2, PData.map(_ -> 0))
      }
      val (sout, out) = problem.solve
      val value       = Double.PositiveInfinity // infeasible
    }

    val VSS = EEV2.value - RP.value // +inf
  }

  // alternative ways to generate nonanticipativity constraints an specify parameter data

  object base {
    // sets (and sets parameters)
    val t = dummy
    val tp = dummy
    val T = set
    val H = param := max(t in T)(t)

    val c = dummy
    val A = set
    val P = set
    val C = set := A | P

    val s = dummy
    val S = set

    // parameters 
    val d = param(T, S)

    val y0 = param
    val ymin = param
    val ymax = param

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
    val y = xvar(T, S) >= 0

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
          h(t) * y(t,s)
        )
      }
    }

    // constraints
    val balance0 = st(                   s in S) { y0        + a(1) + u(1,s) === d(1,s) + w(1,s) + y(1,s) }
    val balance  = st(t in T &~ List(1), s in S) { y(t-1, s) + a(t) + u(t,s) === d(t,s) + w(t,s) + y(t,s) };

    val inventory = st(t in T, s in S) { dlte(ymin, y(t,s), ymax) }

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
    val AData = List("A1", "A2")
    val PData = List("P1", "P2", "P3", "P4")

    val numStages = 3

    val basicMIPWData = basicMIP
      .setData(T, 1 to numStages)
      .setData(A, AData)
      .setData(P, PData)
      .paramData(y0, 20)
      .paramData(ymin, 0)
      .paramData(ymax, 80)
      .paramData(tau, "A1" -> 1, "A2" -> 2)
      .paramData(gamma, PData.map(_ -> 1))
      .paramData(q,  uniform(2)( 10,  50)(AData ::: PData))
      .paramData(ca, uniform(3)(150, 250)(AData ::: PData))
      .paramData(cc, uniform(4)( 30,  50)(AData))
      .paramData(h, (1 to numStages).map(_ -> 1))
  }

  object NA_auto { 
    import base._ 

    // Generated stochastic model
    val stochModel = basicMIPWData.stochastic(T, S, pi)


    // specification of scenarios and paremeters data

    val (t1, t2, t3) = (Stage("1"), Stage("2"), Stage("3"))
    val (init, a, b) = (BasicScenario("init"), BasicScenario("a"), BasicScenario("b"))

    val stochModelStages = stochModel.stochStages(t1, t2, t3)

    // basic tree, two alternatives per stage (dummy probabilities)
    val stochModelBasicScenarios = 
      stochModelStages
        .stochBasicScenarios(t1, init -> r"1")
        .stochBasicScenarios(t2, a -> r"1/2", b -> r"1/2")
        .stochBasicScenarios(t3, a -> r"1/2", b -> r"1/2")

    // final probabilities following Beta[2,2] distribution
    val finalScenarios = stochModelBasicScenarios.finalScenarios
    
    val stochModelFinalProbabilities = 
      stochModelBasicScenarios.stochProbabilities(betaR(2, 2)(finalScenarios))
    
    // demand following U[10,50] distribution
    val scenarios = stochModelBasicScenarios.scenarios
    val uniformDemand = uniformL[Scenario](1)(10, 50)

    val stochModelDemandData = 
      stochModelFinalProbabilities.stochScenarioData(d, uniformDemand(scenarios))

    // mip equivalent
    val mipEquiv = stochModelDemandData.mip

    val modelStr = amphip.sem.mathprog.genModel(mipEquiv.model)
    val dataStr = amphip.sem.mathprog.genData(mipEquiv.data)

    val (sout, out) = mipEquiv.solve
  }

  object NA_byHand {
    import base._ 

    // non-anticipativity by hand-made "adapters"
    val NA_S = set(T)
    val NA_p = param(t in T &~ List(1), NA_S(t)) in NA_S(t-1)

    lazy val NA_pt: ParamStat = 
      param(ind(t in T, s in NA_S(t), tp in T) | tp <= t) in NA_S(tp) :=
        xif (tp === t) { s } { NA_pt(t-1, NA_p(t,s), tp) }

    val NA_ptf = param(s in NA_S(H), t in T) in NA_S(t) := NA_pt(H, s, t)

    val NA_d = param(t in T, NA_S(t))

    val NA_y = xvar(t in T, NA_S(t)) >= 0
    val NA_y_ctr = st(t in T, s in S) { y(t, s) === NA_y(t, NA_ptf(s,t)) }

    val NA_v = xvar(ind(c in P, t in T, NA_S(t)) | t <= H - gamma(c)).binary
    val NA_v_ctr = st(ind(c in P, t in T, s in S) | t <= H - gamma(c)) { v(c,t,s) === NA_v(c,t,NA_ptf(s,t)) }

    val NA_x = xvar(ind(c in A, t in T, NA_S(t)) | t <= tau(c) - 1).binary
    val NA_x_ctr = st(ind(c in A, t in T, s in S) | t <= tau(c) - 1) { x(c,t,s) === NA_x(c,t,NA_ptf(s,t)) }

    // adaptation of parameters and sets
    val d_ = param("d", t in T, s in S) default NA_d(t, NA_ptf(s,t))
    val S_ = set("S") default NA_S(H)

    // the order of replacements is relevant in this case
    val stochModel = (List(NA_ptf, NA_d) ++: basicMIPWData :++ List(NA_y_ctr, NA_v_ctr, NA_x_ctr))
      .replace(d, d_) 
      .replace(S, S_)

    val NA_pi = stochModel.param("pi") // `pi` to use versión with new `S`.


    // specification of scenarios and parameters data

    val uniformDemand = uniform[List[Int]](1)(10, 50)

    val stochModelWData = stochModel
      .setData(NA_S, 
        1 -> List(1),
        2 -> List(1, 2),
        3 -> List(1, 2, 3, 4))
      .paramData(NA_p,
        List(List(2, 1) -> 1, List(2, 2) -> 1) :::
        List(List(3, 1) -> 1, List(3, 2) -> 1, List(3, 3) -> 2, List(3, 4) -> 2)
      )
      .paramData(NA_pi, betaD(2, 2)(1 to 4))
      .paramData(NA_d, 
        uniformDemand(List(List(1, 1))) :::
        uniformDemand(List(List(2, 1), List(2, 2))) :::
        uniformDemand(List(List(3, 1), List(3, 2), List(3, 3), List(3, 4)))
      ) 

    val modelStr = amphip.sem.mathprog.genModel(stochModelWData.model)
    val dataStr = amphip.sem.mathprog.genData(stochModelWData.data)

    val (sout, out) = stochModelWData.solve
  }

  object randomData {
    import support._ 

    // probability distributions for data generation

    /**
     * Given a `seed`, returns a function that given a list of elements
     * and a range, returns a list of pairs with the specified elements 
     * matched with uniform random values in the specified range, using 
     * the seed to initialize the random number generator.
     */
    def uniform[A](seed: Long): (Int, Int) => (Iterable[A]) => Iterable[(A, Int)] = {
      val random = new scala.util.Random()
      random.setSeed(seed)
      
      (min: Int, max: Int) =>
        (xs: Iterable[A]) =>
          xs.map(_ -> random.between(min, max))
    }

    def uniformL[A](seed: Long): (Int, Int) => (Iterable[A]) => Iterable[(A, List[Int])] = { 
      def u[X] = uniform[X](seed)
      
      (min: Int, max: Int) =>
        u[A](min, max).andThen { _.map { case (k, v) => k -> List(v) } }
    }

    def beta[A, N: Numeric](a: Double, b: Double)(xs: Seq[A]): Seq[(A, N)] = {
      import breeze.stats.distributions.Beta
      
      def asN(d: Double): N = Numeric[N].fromDouble(d)

      val betaDist = new Beta(a, b)
      
      val size = xs.size
      val idx = 1 to size
      val idxNorm = idx.map(_.toDouble / size)
      
      val discretization =
        idxNorm
          .sliding(2)
          .map { case Seq(x, y) => asN(betaDist.probability(x, y)) }
          .toSeq
      
      val probs = 
        asN(betaDist.probability(0, idxNorm.head)) +: discretization
      
      xs.zip(probs)
    }

    def betaD[A] = beta[A, Double] _
    def betaR[A] = beta[A, Rational] _ 
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
