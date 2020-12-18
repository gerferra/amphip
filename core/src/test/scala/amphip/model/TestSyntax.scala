package amphip.model

import scalaz.{Forall => _, _}, Scalaz._, scalaz.syntax._

import amphip.dsl._
import amphip.model.ast._

/*
 * Expressions to exercise MathProg support.
 * TODO add type asserts
 */
object TestSyntax {

  val I = set("I")
  val J = set("J") dimen 3 within I
  val K = set("K") := J

  val S = set("S")

  val cant = param("cant").integer

  val N = set("N") := 1 to cant

  val a = param("a").integer
  val b = param("b") < 8
  val c = param("c").symbolic := "abc"

  val x = xvar("x")
  val y = xvar("y").binary
  val s = dummy("s")
  val i = dummy("i")
  val j = dummy("j")

  val z = xvar("z", ind(I, J))

  b in I

  tup(a, b) in I * J
  s in S

  i in I

  List(i, j) in I * J

  b() in I

  List(a(), b()) in J * I
  tup(j, b()) in J * I
  tup(j, b) in J * I

  val r1: ArithSet = BigDecimal(1) to 2.5 by 0.1
  val r2: ArithSet = 1 to 2

  val s1: SetExpr = List(List(a, b), List(a, c))

  s1.shows
  val s2: SetExpr = List(List(1, 2), List(3, 4))
  s2.shows

  val range = a to 2

  range by 4

  sum(ind(I))(i + x)
  ind(I, J)
  (ind(i in I, j in J) | i <= j && i =!= 3).shows

  z(i, j)
  z(List(i, j))
  z(List(i, j))

  {
    /* 
     * this usages of "+" with a String argument needs that any2stringadd be disabled ... 
     * calls the method to avoid warning ...
     */
    def any2stringadd(): Unit = (); any2stringadd()

    i + "a"

    str('a') + "a"

    str('a') + b
  }

  i + 'a'
  i + 1
  b + 'a'
  x + y

  x() + y()

  sum(i in I)(c(i) * x(i)) + sum(j in J)(j * y(j))

  sum(i in I, j in J)(i - j)

  sum(I, J)(i less j).shows

  xif(a > b)(1)(3)

  xif1(a > b)(1)

  xif1(a || b)(1)
  xif(a > b)("1")(1)

  xif1(i in I)("a")

  xif1(a > b)("a")
  xif(a > b)(I)(J)

  I within J

  !(I() within J())

  (x === y)

  (x() === y())

  //x * y
  //-x / +y
  -x / +b
  //-x /~ +y

  !(a === b)

  a =!= b
  a * b
  -a ** +b
  -a / +b
  -a /~ +b
  -a % +b

  !(a() === b())
  a() * b()
  -a() ** +b()
  -a() / +b()

  -a() /~ +b()
  -a() % +b()

  !(a() =!= b())

  a === y
  a * y

  a() === y()
  a() * y()

  x === b
  x * b

  -x / +b

  x() === b()
  x() * b()

  -x() / +b()

  (-x() / +b()).shows

  x <= y

  x() <= y()

  a <= b

  a() <= b()

  a <= y
  a() <= y()

  x <= b

  a() < b

  /* 
   this fails because `x <= b' returns a new `VarStat' with the constraint as an attribute, 
   so one can do things like `val x = xvar("x") >= b'. 
   In this case is needed to make explicit that we are talking about a `VarRef', ie, add parenthesis to `x'
    
    st("something")(x <= b) 
  */
  st("something")(x() <= b)

  x >= y

  x() >= y()

  a >= b

  !(a() >= b())

  a >= y

  a() >= y()

  x >= b

  x() >= b()

  minimize { x * 3 - 2 }

  maximize { x }

  // a * x >= a * x

  val D1 = set("D1") default Nil

  // hack to obtain "an empty set of dimension two" in MathProg
  val D2 = set("D2") within D1 * D1 default ind(Nil, Nil);

}
