/*
 * Simplified version of the deterministic model in
 * http://dx.doi.org/10.1590/0101-7438.2019.039.01.0037.
 *
 */
import amphip.dsl._

object fuelSupplyMinDet {

  // sets (and sets parameters)
  val t = dummy
  val tp = dummy
  val H = param
  val T = set := 1 to H

  val c = dummy
  val A = set
  val P = set
  val C = set := A | P

  // parameters (and sets derived from parameters)
  val d = param(T)

  val k0 = param
  val kmin = param
  val kmax = param

  val tau = param(A) in T
  val gamma = param(P) in (0 to H - 1)

  val q = param(C)

  val ca = param(C) 
  val cc = param(A)
  val h = param(T)

  val a = param(t in T) :=
    sum(ind(c in A) | tau(c) === t) { q(c) }

  // variables
  val k = xvar(T) >= 0

  val v = xvar(ind(c in P, t in T) | t <= H - gamma(c)).binary
  val x = xvar(ind(c in A, t in T) | t <= tau(c) - 1).binary

  val u = xvar(T) >= 0
  val w = xvar(T) >= 0

  // objective function
  val cost = minimize {
    sum(t in T) {
      sum(ind(c in P) | t <= H - gamma(c)) {  ca(c)          * q(c) * v(c,t) } +
      sum(ind(c in A) | t <= tau(c) - 1  ) { (cc(c) - ca(c)) * q(c) * x(c,t) } +
      h(t) * k(t)
    }
  }

  // constraints
  val balance0 = st                   { k0     + a(1) + u(1) === d(1) + w(1) + k(1) }
  val balance  = st(ind(t in T) | t > 1) { k(t-1) + a(t) + u(t) === d(t) + w(t) + k(t) }

  val inventory = st(t in T) { dlte(kmin, k(t), kmax) }

  val singleAcquisition  = st(c in P) { sum(ind(t in T) | t <= H - gamma(c)) { v(c,t) } <= 1 }
  val singleCancellation = st(c in A) { sum(ind(t in T) | t <= tau(c) - 1  ) { x(c,t) } <= 1 }

  val acquiredFuel = st(t in T) {
    u(t) === sum(ind(c in P) | gamma(c) <= t-1) { q(c) * v(c, t-gamma(c)) }
  }

  val cancelledFuel = st(t in T) {
    w(t) === sum(ind(c in A) | tau(c) === t) { q(c) * sum(ind(tp in T) | tp <= tau(c) - 1) { x(c,tp) } }
  }

  // model
  val basicMIP = 
    model(cost, 
      balance0, balance, inventory, singleAcquisition, singleCancellation, acquiredFuel, cancelledFuel)

  // data
  import randomData._

  val AData = List("A1", "A2")
  val PData = List("P1", "P2", "P3", "P4")

  val numStages = 3

  val basicMIPWData = basicMIP
    .paramData(H, 3)
    .setData(A, AData)
    .setData(P, PData)
    .paramData(k0, 20)
    .paramData(kmin, 0)
    .paramData(kmax, 80)
    .paramData(tau, "A1" -> 1, "A2" -> 2)
    .paramData(gamma, PData.map(_ -> 1))
    .paramData(d,  1 -> 35.0, 2 -> 27.5, 3 -> 36.25)
    .paramData(q,  uniform(2)( 10,  50)(AData ::: PData))
    .paramData(ca, uniform(3)(150, 250)(AData ::: PData))
    .paramData(cc, uniform(4)( 30,  50)(AData))
    .paramData(h, (1 to numStages).map(_ -> 1))

  val (sout, out) = basicMIPWData.solve


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
