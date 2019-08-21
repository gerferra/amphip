package amphip.model

import scalaz.{ Failure => _, Success => _, Forall => _, _ }, Scalaz._

import org.junit.Assert._
import org.junit.Test

import amphip.base._
import amphip.model.ast._
import amphip.model.Parser._
import amphip.model.Parser.Implicits._

class TestParser {

  val p = Parser()

  import p._

  implicit class TestRun[A](result: ParseResult[S[A]]) {

    def isTypeMismatch(implicit symTab: SymTab): Boolean = result.run2 match {
      case -\/(-\/(_)) => false
      case -\/(\/-(err)) => err.isInstanceOf[TypeMismatch]
      case \/-(_) => false
    }

  }

  @Test
  def test(): Unit = {

    def mkSymTab(stats: Decl*): SymTab = SymTab(LinkedMap(stats.map(x => x.name -> x): _*))

    val A = SetStat("A")
    val B = SetStat("B")
    val B1 = SetStat("B1")
    val C = SetStat("C")
    val D = SetStat("D")
    val E = SetStat("E")
    val F = SetStat("F")
    val I = SetStat("I")
    val I1 = SetStat("I1")
    val J = SetStat("J")
    val J1 = SetStat("J1")
    val S = SetStat("S")
    //val step = SetStat("step")
    val T = SetStat("T")
    val U = SetStat("U")
    val V = SetStat("V")
    val X = SetStat("X")
    val Y = SetStat("Y")

    val a = ParamStat("a")
    val alpha = ParamStat("alpha")
    val b = ParamStat("b")
    val c = ParamStat("c")
    val city = ParamStat("city")
    //val comb = ParamStat("comb")
    val commit = ParamStat("commit")
    val M = ParamStat("M")
    val market = ParamStat("market")
    val N = ParamStat("N")
    //val maxiter = ParamStat("maxiter")
    val name = ParamStat("name")
    val p = ParamStat("p")
    val prd = SetStat("prd")
    val q = ParamStat("q")
    val raw = SetStat("raw")
    val test = ParamStat("test")
    val time = ParamStat("time")
    val x = ParamStat("x")
    val y = ParamStat("y")
    val year = ParamStat("year")

    val w = VarStat("w")
    val z = VarStat("z")

    implicit val symTab: SymTab = mkSymTab(A, B, B1, C, D, E, F, I, I1, J, J1, S, T, U, V, X, Y, a, alpha, b, c, city, commit, M, market, N, name, p, prd, q, raw, test, time, x, y, year, z, w)

    val i = DummyIndDecl("i")
    val j = DummyIndDecl("j")
    val k = DummyIndDecl("k")
    val l = DummyIndDecl("l")
    val s = DummyIndDecl("s")
    val s1 = DummyIndDecl("s1")
    val s2 = DummyIndDecl("s2")
    val t = DummyIndDecl("t")
    val t1 = DummyIndDecl("t1")
    val t2 = DummyIndDecl("t2")
    val x1 = DummyIndDecl("x1")
    val x2 = DummyIndDecl("x2")

    val d0_ = DummyIndDecl("d_", synthetic = true)
    val d1_ = DummyIndDecl("d1_", synthetic = true)
    val d2_ = DummyIndDecl("d2_", synthetic = true)
    val d3_ = DummyIndDecl("d3_", synthetic = true)
    val d4_ = DummyIndDecl("d4_", synthetic = true)
    val d5_ = DummyIndDecl("d5_", synthetic = true)
    val d6_ = DummyIndDecl("d6_", synthetic = true)
    val d7_ = DummyIndDecl("d7_", synthetic = true)

    import scala.language.implicitConversions
    implicit def AsRef(d: DummyIndDecl): DummyIndRef = DummyIndRef(d)

    assertEquals(
      parse(phrase(pSymName), """
  /*  xxxx
      yyyyy zzzz
      \n
      "
      //dd,dd,d
     */


     # test ### /* */

     /* a */

     /*
       a
       b
     */
     A
     """).get, "A")

    assertEquals(
      parse(phrase(pSetStat), """set s; # This is a comment""").runR, SetStat("s"))

    assertEquals(parse(phrase(pSymName), "A").get, "A")

    assertEquals(parse(phrase(pSymName), "alpha123").get, "alpha123")

    assertEquals(parse(phrase(pSymName), "This_is_a_name").get, "This_is_a_name")

    assertEquals(parse(phrase(pSymName), "_P123_abc_321").get, "_P123_abc_321")

    assertEquals(parse(phrase(pNumLit), "123").get, NumLit(123))

    assertEquals(parse(phrase(pNumLit), "3.14159").get, NumLit(3.14159))

    assertTrue(parse(phrase(pNumLit), "56.E+5").isInstanceOf[Failure])

    assertEquals(parse(phrase(pNumLit), ".78").get, NumLit(.78))

    assertEquals(parse(phrase(pNumLit), "123.456e-7").get, NumLit(123.456e-7))

    assertEquals(parse(phrase(pNumLit), "0").get, NumLit(0))

    assertEquals(parse(phrase(pNumLit), "0.00").get, NumLit(0.00))

    assertEquals(parse(phrase(pNumLit), "-123").get, NumLit(-123))

    assertEquals(parse(phrase(pNumLit), "1.23E3").get, NumLit(1.23E3))

    assertEquals(parse(phrase(pNumLit), "1.23E+3").get, NumLit(1.23E+3))

    assertEquals(parse(phrase(pNumLit), "12.3E+7").get, NumLit(12.3E+7))

    assertEquals(parse(phrase(pNumLit), "12.3").get, NumLit(12.3))

    assertEquals(parse(phrase(pNumLit), "0.00123").get, NumLit(0.00123))

    assertEquals(parse(phrase(pNumLit), "-1.23E-12").get, NumLit(-1.23E-12))

    assertEquals(parse(phrase(pNumLit), "-1.23e-12").get, NumLit(-1.23E-12))

    assertEquals(parse(phrase(pNumLit), "1234.5E-4").get, NumLit(1234.5E-4))

    assertEquals(parse(phrase(pNumLit), "1234.5e-4").get, NumLit(1234.5E-4))

    assertEquals(parse(phrase(pNumLit), "1234.5d-4").get, NumLit(1234.5E-4))

    assertEquals(parse(phrase(pNumLit), "1234.5D-4").get, NumLit(1234.5E-4))

    assertEquals(parse(phrase(pNumLit), "0E+7").get, NumLit(0E+7))

    assertEquals(parse(phrase(pNumLit), "-0").get, NumLit(-0))

    assertEquals(parse(phrase(pNumLit), "+0").get, NumLit(+0))

    assertTrue(parse(phrase(pNumLit), """ 1. """).isInstanceOf[Failure])

    assertEquals(parse(phrase(pNumLit), """ 1.1 """).get, NumLit(1.1))

    assertEquals(parse(phrase(pNumLit), """ .1 """).get, NumLit(.1))

    assertEquals(parse(phrase(pStringLit), """'This is a string'""").get, StringLit("""This is a string"""))

    assertEquals(parse(phrase(pStringLit), """"This is another string"""").get, StringLit("""This is another string"""))

    assertEquals(parse(phrase(pStringLit), """'1 + 2 = 3'""").get, StringLit("""1 + 2 = 3"""))

    assertEquals(parse(phrase(pStringLit), """'That''s all'""").get, StringLit("""That's all"""))

    assertEquals(parse(phrase(pStringLit), """"She said: ""No"""""").get, StringLit("""She said: "No""""))

    assertEquals(parse(phrase(pStringLit), """  " A_abc_ "  """).get, StringLit(""" A_abc_ """))

    assertEquals(parse(phrase(pStringLit), """  ' A_abc_ '  """).get, StringLit(""" A_abc_ """))

    assertTrue(parse(phrase(pStringLit), """  " A_abc_ '  """).isInstanceOf[Failure])

    assertTrue(parse(phrase(pStringLit), """  ' A_abc_ "  """).isInstanceOf[Failure])

    assertEquals(parse(phrase(pStringLit), """  " A_ab'c_ "  """).get, StringLit(""" A_ab'c_ """))

    assertEquals(parse(phrase(pStringLit), """  ' A_ab"c_ '  """).get, StringLit(""" A_ab"c_ """))

    assertTrue(parse(phrase(pStringLit), """  " A_ab"c_ "  """).isInstanceOf[Failure])

    assertEquals(parse(phrase(pStringLit), """  " A_ab"" ""c_ "  """).get, StringLit(""" A_ab" "c_ """))

    assertTrue(parse(phrase(pStringLit), """  ' A_ab'c_ '  """).isInstanceOf[Failure])

    assertEquals(parse(phrase(pStringLit), """  ' A_ab''c_ '  """).get, StringLit(""" A_ab'c_ """))

    assertEquals(parse(phrase(pStringLit), """  ' A_ab''''c''_ '  """).get, StringLit(""" A_ab''c'_ """))

    assertEquals(parse(phrase(pReservedKeyword), "else").get, "else")

    assertTrue(parse(phrase(pSymName), "else").isInstanceOf[Failure])

    assertEquals(parse(phrase(pNumExpr), "1.23").runR, NumLit(1.23))

    assertEquals(parse(phrase(pNumExpr), "p").runR, ParamRef(p))

    assertEquals(parse(phrase(pNumExpr), "time").runR, ParamRef(time))

    assertEquals(
      parse(phrase(pNumExpr), "a['May 2003', p, 1]").runR,
      ParamRef(a, List(StringLit("May 2003"), ParamRef(p), NumLit(1))))

    assertEquals(
      parse(phrase(pNumExpr), "abs(-3)").runR,
      Abs(NumLit(-3)))

    assertEquals(
      parse(phrase(pNumExpr), "atan(3)").runR,
      Atan(NumLit(3)))

    assertEquals(
      parse(phrase(pNumExpr), "atan(3,2)").runR,
      Atan2(NumLit(3), NumLit(2)))

    assertEquals(
      parse(phrase(pNumExpr), "card(X)").runR,
      Card(SetRef(X)))

    assertEquals(
      parse(phrase(pNumExpr), "ceil(3)").runR,
      Ceil(NumLit(3)))

    assertEquals(
      parse(phrase(pNumExpr), "cos(3)").runR,
      Cos(NumLit(3)))

    assertEquals(
      parse(phrase(pNumExpr), "exp(3)").runR,
      Exp(NumLit(3)))

    assertEquals(
      parse(phrase(pNumExpr), "floor(3)").runR,
      Floor(NumLit(3)))

    assertEquals(
      parse(phrase(pNumExpr), "gmtime()").runR,
      Gmtime())

    assertEquals(
      parse(phrase(pNumExpr), "length('abc')").runR,
      Length(StringLit("abc")))

    assertEquals(
      parse(phrase(pNumExpr), "log(5)").runR,
      Log(NumLit(5)))

    assertEquals(
      parse(phrase(pNumExpr), "log10(5)").runR,
      Log10(NumLit(5)))

    assertEquals(
      parse(phrase(pNumExpr), "max(1,2,3)").runR,
      Max(NumLit(1), NumLit(2), NumLit(3)))

    assertEquals(
      parse(phrase(pNumExpr), "min(1,2,3)").runR,
      Min(NumLit(1), NumLit(2), NumLit(3)))

    assertEquals(
      parse(phrase(pNumExpr), "round(4.5)").runR,
      Round(NumLit(4.5)))

    assertEquals(
      parse(phrase(pNumExpr), "round(4.5, 1)").runR,
      Round(NumLit(4.5), Some(NumLit(1))))

    assertEquals(
      parse(phrase(pNumExpr), "sqrt(2)").runR,
      Sqrt(NumLit(2)))

    assertEquals(
      parse(phrase(pNumExpr), "str2time('some time', 'some format')").runR,
      Str2time(StringLit("some time"), StringLit("some format")))

    assertEquals(
      parse(phrase(pNumExpr), "trunc(4.5)").runR,
      Trunc(NumLit(4.5)))

    assertEquals(
      parse(phrase(pNumExpr), "trunc(4.5, 1)").runR,
      Trunc(NumLit(4.5), Some(NumLit(1))))

    assertEquals(
      parse(phrase(pNumExpr), "Irand224()").runR,
      Irand224())

    assertEquals(
      parse(phrase(pNumExpr), "Uniform01()").runR,
      Uniform01())

    assertEquals(
      parse(phrase(pNumExpr), "sum{i in S} sum{j in J} i * j").runR,
      NumSum(
        IndExpr(List(IndEntry(List(i), SetRef(S))), None),
        NumSum(
          IndExpr(List(IndEntry(List(j), SetRef(J))), None),
          NumMult(i, j))))

    assertEquals(
      parse(phrase(pNumExpr), "(3 * sum{i in S} sum{j in J} i * j) * 5").runR,
      NumMult(
        NumMult(
          NumLit(3),
          NumSum(IndExpr(List(IndEntry(List(i), SetRef(S))), None),
            NumSum(IndExpr(List(IndEntry(List(j), SetRef(J))), None),
              NumMult(i, j)))),
        NumLit(5)))

    assertEquals(
      parse(phrase(pNumExpr), "7 + sum{s in S} p[s, t] + 8").runR(symTab + t),
      NumAdd(
        NumAdd(
          NumLit(7),
          NumSum(
            IndExpr(List(IndEntry(List(s), SetRef(S))), None),
            ParamRef(p, List(s, t)))),
        NumLit(8)))

    assertEquals(
      parse(phrase(pNumExpr), "sum{i in S} alpha[i]").runR,
      NumSum(IndExpr(List(IndEntry(List(i), SetRef(S)))), ParamRef(alpha, List(i))))

    assertEquals(
      parse(phrase(pNumExpr), """-sum{i in I} i""").runR,
      NumUnaryMinus(NumSum(IndExpr(List(IndEntry(List(i), SetRef(I)))), i)))

    assertEquals(
      parse(phrase(pNumExpr), "prod{i in S} alpha[i]").runR,
      NumProd(IndExpr(List(IndEntry(List(i), SetRef(S)))), ParamRef(alpha, List(i))))

    assertEquals(
      parse(phrase(pNumExpr), "min{i in S} alpha[i]").runR,
      NumMin(IndExpr(List(IndEntry(List(i), SetRef(S)))), ParamRef(alpha, List(i))))

    assertEquals(
      parse(phrase(pNumExpr), "max{i in S} alpha[i]").runR,
      NumMax(IndExpr(List(IndEntry(List(i), SetRef(S)))), ParamRef(alpha, List(i))))

    assertEquals(
      parse(phrase(pNumExpr), "if i in I then 2 else q[i]").runR(symTab + i),
      CondNumExpr(
        In(List(i), SetRef(I)),
        NumLit(2),
        ParamRef(q, List(i)).some))

    assertEquals(
      parse(phrase(pNumExpr), "if i in I then 2").runR(symTab + i),
      CondNumExpr(
        In(List(i), SetRef(I)),
        NumLit(2)))

    assertEquals(
      parse(phrase(pNumExpr), "2 ** 3 ** 4 ** 5").runR,
      NumRaise(NumLit(2), NumRaise(NumLit(3), NumRaise(NumLit(4), NumLit(5)))))

    assertEquals(
      parse(phrase(pNumExpr), "(2 ** 3) ** 4 ** 5").runR,
      NumRaise(NumRaise(NumLit(2), NumLit(3)), NumRaise(NumLit(4), NumLit(5))))

    assertEquals(
      parse(phrase(pNumExpr), "-q").runR,
      NumUnaryMinus(ParamRef(q)))

    assertEquals(
      parse(phrase(pNumExpr), "+q").runR,
      NumUnaryPlus(ParamRef(q)))

    assertEquals(
      parse(phrase(pNumExpr), "+a * -c").runR,
      NumMult(
        NumUnaryPlus(ParamRef(a)),
        NumUnaryMinus(ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "+a / -c").runR,
      NumDiv(
        NumUnaryPlus(ParamRef(a)),
        NumUnaryMinus(ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "+a div -c").runR,
      NumDivExact(
        NumUnaryPlus(ParamRef(a)),
        NumUnaryMinus(ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "+a mod -c").runR,
      NumMod(
        NumUnaryPlus(ParamRef(a)),
        NumUnaryMinus(ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "7 * (2 ** 3) ** 4 ** 5 * -a").runR,
      NumMult(NumMult(NumLit(7), NumRaise(NumRaise(NumLit(2), NumLit(3)), NumRaise(NumLit(4), NumLit(5)))), NumUnaryMinus(ParamRef(a))))

    assertEquals(
      parse(phrase(pNumExpr), "1 * 2 / 3 div 4 mod 5").runR,
      NumMod(NumDivExact(NumDiv(NumMult(NumLit(1), NumLit(2)), NumLit(3)), NumLit(4)), NumLit(5)))

    assertEquals(
      parse(phrase(pNumExpr), "1 mod 2 div 3 / 4 * 5").runR,
      NumMult(NumDiv(NumDivExact(NumMod(NumLit(1), NumLit(2)), NumLit(3)), NumLit(4)), NumLit(5)))

    assertEquals(
      parse(phrase(pNumExpr), "+a + -c").runR,
      NumAdd(
        NumUnaryPlus(ParamRef(a)),
        NumUnaryMinus(ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "+a - -c").runR,
      NumSub(
        NumUnaryPlus(ParamRef(a)),
        NumUnaryMinus(ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "+a less -c").runR,
      NumLess(
        NumUnaryPlus(ParamRef(a)),
        NumUnaryMinus(ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "(b[i, j] + .5 * c)").runR(symTab + i + j),
      NumAdd(
        ParamRef(b, List(i, j)),
        NumMult(NumLit(0.5), ParamRef(c))))

    assertEquals(
      parse(phrase(pNumExpr), "if -a > 2 then sum{i in I} x[i] + 8 else max{i in I} x[i] + 9").runR,
      CondNumExpr(
        GT(NumUnaryMinus(ParamRef(a)), NumLit(2)),
        NumAdd(
          NumSum(
            IndExpr(List(IndEntry(List(i), SetRef(I))), None),
            ParamRef(x, List(i))),
          NumLit(8)),
        NumAdd(
          NumMax(
            IndExpr(List(IndEntry(List(i), SetRef(I))), None),
            ParamRef(x, List(i))),
          NumLit(9)).some))

    assertEquals(
      parse(phrase(pNumExpr), "if -a > 2 then if +b < -8 then 10 else 20 else if +b >= -8 then 30 else 40").runR,
      CondNumExpr(
        GT(NumUnaryMinus(ParamRef(a)), NumLit(2)),
        CondNumExpr(
          LT(NumUnaryPlus(ParamRef(b)), NumLit(-8)),
          NumLit(10),
          NumLit(20).some),
        CondNumExpr(
          GTE(NumUnaryPlus(ParamRef(b)), NumLit(-8)),
          NumLit(30),
          NumLit(40).some).some))

    assertEquals(
      parse(phrase(pNumExpr), "if -a > 2 then (if +b < -8 then 10) else 20").runR,
      CondNumExpr(
        GT(NumUnaryMinus(ParamRef(a)), NumLit(2)),
        CondNumExpr(
          LT(NumUnaryPlus(ParamRef(b)), NumLit(-8)),
          NumLit(10)),
        NumLit(20).some))

    assertEquals(
      parse(phrase(pNumExpr), "if -a > 2 then if +b < -8 then 10 else 20").runR,
      CondNumExpr(
        GT(NumUnaryMinus(ParamRef(a)), NumLit(2)),
        CondNumExpr(
          LT(NumUnaryPlus(ParamRef(b)), NumLit(-8)),
          NumLit(10),
          NumLit(20).some)))

    assertEquals(
      parse(phrase(pSymExpr), "'May 2003'").runR,
      StringLit("May 2003"))

    assertEquals(
      parse(phrase(pSymExpr), "j").runR(symTab + j), j: SymExpr)

    assertEquals(
      parse(phrase(pSymExpr), "p").runR,
      ParamRef(p))

    assertEquals(
      parse(phrase(pSymExpr), "x['abc', j+1]").runR(symTab + j),
      ParamRef(x, List(StringLit("abc"), NumAdd(j, NumLit(1)))))

    assertEquals(
      parse(phrase(pSymExpr), "substr(name[i], k+1, 3)").runR(symTab + i + k),
      Substr(ParamRef(name, List(i)), NumAdd(k, NumLit(1)), Some(NumLit(3))))

    assertEquals(
      parse(phrase(pSymExpr), "substr(name[i], k+1)").runR(symTab + i + k),
      Substr(ParamRef(name, List(i)), NumAdd(k, NumLit(1))))

    assertEquals(
      parse(phrase(pSymExpr), "time2str(123, 'format')").runR,
      Time2str(NumLit(123), StringLit("format")))

    assertEquals(
      parse(phrase(pSymExpr), "'abc[' & i & ',' & j & ']'").runR(symTab + i + j),
      Concat(
        Concat(
          Concat(
            Concat(StringLit("abc["), i),
            StringLit(",")),
          j),
        StringLit("]")))

    assertEquals(
      parse(phrase(pSymExpr), """ "from " & city[i] & " to " & city[j] """).runR(symTab + i + j),
      Concat(
        Concat(
          Concat(StringLit("from "), ParamRef(city, List(i))),
          StringLit(" to ")),
        ParamRef(city, List(j))))

    assertEquals(
      parse(phrase(pSymExpr), "if i in I then x[i,j] else y[i+1]").runR(symTab + i + j),
      CondSymExpr(
        In(List(i), SetRef(I)),
        ParamRef(x, List(i, j)),
        ParamRef(y, List(NumAdd(i, NumLit(1)))).some))

    assertEquals(
      parse(phrase(pSymExpr), "'begin' & if -a > 2 then 'middle-end' & test & if -a <= 2 then '-end' & test else '?'").runR,
      Concat(
        StringLit("begin"),
        CondSymExpr(
          GT(NumUnaryMinus(ParamRef(a)), NumLit(2)),
          Concat(
            Concat(StringLit("middle-end"), ParamRef(test)),
            CondSymExpr(
              LTE(NumUnaryMinus(ParamRef(a)), NumLit(2)),
              Concat(StringLit("-end"), ParamRef(test)),
              StringLit("?").some)))))

    assertEquals(
      parse(phrase(pSymExpr), "'begin' & 8 + if -a > 2 then 16  & if -a <= 2 then 8").runR,
      Concat(
        Concat(
          StringLit("begin"),
          SymNumExpr(
            NumAdd(
              NumLit(8),
              CondNumExpr(
                GT(NumUnaryMinus(ParamRef(a)), NumLit(2)),
                NumLit(16))))),
        SymNumExpr(
          CondNumExpr(
            LTE(NumUnaryMinus(ParamRef(a)), NumLit(2)),
            NumLit(8)))))

    assertEquals(
      parse(phrase(pSymExpr), "((10 * b[i,j]) & '.bis')").runR(symTab + i + j),
      Concat(
        SymNumExpr(NumMult(NumLit(10), ParamRef(b, List(i, j)))),
        StringLit(".bis")))

    assertEquals(
      parse(phrase(pIndExpr), "{s in S, t in T}").runR,
      IndExpr(
        List(
          IndEntry(List(s), SetRef(S)),
          IndEntry(List(t), SetRef(T))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{(s1, s2) in S, t in T}").runR,
      IndExpr(
        List(
          IndEntry(List(s1, s2), SetRef(S)),
          IndEntry(List(t), SetRef(T))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{S, t in T}").runR,
      IndExpr(
        List(
          IndEntry(Nil, SetRef(S)),
          IndEntry(List(t), SetRef(T))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{s in S, (t1, t2) in T}").runR,
      IndExpr(
        List(
          IndEntry(List(s), SetRef(S)),
          IndEntry(List(t1, t2), SetRef(T))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{s in S, T}").runR,
      IndExpr(
        List(
          IndEntry(List(s), SetRef(S)),
          IndEntry(Nil, SetRef(T))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{i in A, (j,k) in B, l in C}").runR,
      IndExpr(
        List(
          IndEntry(List(i), SetRef(A)),
          IndEntry(List(j, k), SetRef(B)),
          IndEntry(List(l), SetRef(C))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{A, B, C}").runR,
      IndExpr(
        List(
          IndEntry(List(), SetRef(A)),
          IndEntry(List(), SetRef(B)),
          IndEntry(List(), SetRef(C))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{i in A, (i-1,k) in B, l in C}").runR,
      IndExpr(
        List(
          IndEntry(List(i), SetRef(A)),
          IndEntry(List(d0_, k), SetRef(B), predicate = Eq(d0_, NumSub(i, NumLit(1))).some), // means `"j" == i-1` because it's an expression
          IndEntry(List(l), SetRef(C))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{i in A, (i,k) in B, l in C}").runR,
      IndExpr(
        List(
          IndEntry(List(i), SetRef(A)),
          IndEntry(List(d1_, k), SetRef(B), predicate = Eq(d1_, i).some), // means `j == i` because `i` already declared
          IndEntry(List(l), SetRef(C))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{i in A, (p[1] - 1,k) in B, l in C}").runR,
      IndExpr(
        List(
          IndEntry(List(i), SetRef(A)),
          IndEntry(List(d2_, k), SetRef(B), predicate = Eq(d2_, NumSub(ParamRef(p, List(NumLit(1))), NumLit(1))).some),
          IndEntry(List(l), SetRef(C))),
        None))

    /*
    expressions in `DummyInd` position at `IndEntry` can only reference other `DummyInd` declared in other `IndEntry` of the same `IndExpr`
  */

    assertEquals(
      parse(phrase(pIndExpr), "{i in A, (j, j+1) in B, l in C}").runR,
      IndExpr(
        List(
          IndEntry(List(i), SetRef(A)),
          IndEntry(List(j, d3_), SetRef(B), predicate = Eq(d3_, NumAdd(j, NumLit(1))).some), // parseable but it's an error, `j` is not defined (must be defined in other IndEntry)
          IndEntry(List(l), SetRef(C))),
        None))

    assertEquals(
      parse(phrase(pIndExpr), "{i in A, (j,k) in B, l in C: i <= 5 and k <> 'Mar'}").runR,
      IndExpr(
        List(
          IndEntry(List(i), SetRef(A)),
          IndEntry(List(j, k), SetRef(B)),
          IndEntry(List(l), SetRef(C))),
        Some(Conj(LTE(i, NumLit(5)), NEq(k, StringLit("Mar"))))))

    assertEquals(
      parse(phrase(pSetExpr), "{s in S, t in T}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(s), SetRef(S)),
            IndEntry(List(t), SetRef(T))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{(s1, s2) in S, t in T}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(s1, s2), SetRef(S)),
            IndEntry(List(t), SetRef(T))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{S, t in T}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(Nil, SetRef(S)),
            IndEntry(List(t), SetRef(T))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{s in S, (t1, t2) in T}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(s), SetRef(S)),
            IndEntry(List(t1, t2), SetRef(T))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{s in S, T}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(s), SetRef(S)),
            IndEntry(Nil, SetRef(T))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{i in A, (j,k) in B, l in C}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(A)),
            IndEntry(List(j, k), SetRef(B)),
            IndEntry(List(l), SetRef(C))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{A, B, C}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(), SetRef(A)),
            IndEntry(List(), SetRef(B)),
            IndEntry(List(), SetRef(C))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{i in A, (i-1,k) in B, l in C}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(A)),
            IndEntry(List(d4_, k), SetRef(B), predicate = Eq(d4_, NumSub(i, NumLit(1))).some), // means `"j" == i-1` because it's an expression
            IndEntry(List(l), SetRef(C))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{i in A, (i,k) in B, l in C}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(A)),
            IndEntry(List(d5_, k), SetRef(B), predicate = Eq(d5_, i).some), // means `j == i` because `i` already declared
            IndEntry(List(l), SetRef(C))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{i in A, (p[1] - 1,k) in B, l in C}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(A)),
            IndEntry(List(d6_, k), SetRef(B), predicate = Eq(d6_, NumSub(ParamRef(p, List(NumLit(1))), NumLit(1))).some),
            IndEntry(List(l), SetRef(C))),
          None)))

    /*
    expressions in `DummyInd` position at `IndEntry` can only reference other `DummyInd` declared in other `IndEntry` of the same `IndExpr`
  */

    assertEquals(
      parse(phrase(pSetExpr), "{i in A, (j, j+1) in B, l in C}").runR,
      IndExprSet(

        IndExpr(
          List(
            IndEntry(List(i), SetRef(A)),
            IndEntry(List(j, d7_), SetRef(B), predicate = Eq(d7_, NumAdd(j, NumLit(1))).some), // parseable but it's an error, `j` is not defined (must be defined in other IndEntry)
            IndEntry(List(l), SetRef(C))),
          None)))

    assertEquals(
      parse(phrase(pSetExpr), "{i in A, (j,k) in B, l in C: i <= 5 and k <> 'Mar'}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(A)),
            IndEntry(List(j, k), SetRef(B)),
            IndEntry(List(l), SetRef(C))),
          Some(Conj(LTE(i, NumLit(5)), NEq(k, StringLit("Mar")))))))

    assertEquals(
      parse(phrase(pSetExpr), "{(123, 'aa'), (i, 'bb'), (j-1, 'cc')}").runR(symTab + i + j),
      SetLit(
        List(NumLit(123), StringLit("aa")),
        List(i, StringLit("bb")),
        List(NumSub(j, NumLit(1)), StringLit("cc"))))

    assertEquals(
      parse(phrase(pSetExpr), "{ 123, i, j-1 }").runR(symTab + i + j),
      SetLit(
        List(NumLit(123)),
        List(i),
        List(NumSub(j, NumLit(1)))))

    assertEquals(
      parse(phrase(pSetExpr), "{}").runR,
      SetLit())

    assertEquals(
      parse(phrase(pSetExpr), "{{}, {}}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(Nil, SetLit()),
            IndEntry(Nil, SetLit())))))

    assertEquals(
      parse(phrase(pSetExpr), "{i in {}, j in {}}").runR,
      IndExprSet(
        IndExpr(
          List(
            IndEntry(List(i), SetLit()),
            IndEntry(List(j), SetLit())))))

    assertEquals(parse(phrase(pSetExpr), "I").runR, SetRef(I))

    assertEquals(
      parse(phrase(pSetExpr), "S[i-1, j+1]").runR(symTab + i + j),
      SetRef(S, List(
        NumSub(i, NumLit(1)),
        NumAdd(j, NumLit(1)))))

    assertEquals(
      parse(phrase(pSetExpr), "1 .. x-1 by 2").runR,
      ArithSet(
        NumLit(1),
        NumSub(ParamRef(x), NumLit(1)),
        Some(NumLit(2))))

    assertEquals(
      parse(phrase(pSetExpr), "1..x-1 by 2").runR,
      ArithSet(
        NumLit(1),
        NumSub(ParamRef(x), NumLit(1)),
        Some(NumLit(2))))

    assertEquals(
      parse(phrase(pSetExpr), "setof{i in I, j in J} (i+1, j-1)").runR,
      SetOf(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(I)),
            IndEntry(List(j), SetRef(J))),
          None),
        List(
          NumAdd(i, NumLit(1)),
          NumSub(j, NumLit(1)))))

    assertEquals(
      parse(phrase(pSetExpr), "setof{i in I, j in J} i+j").runR,
      SetOf(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(I)),
            IndEntry(List(j), SetRef(J))),
          None),
        List(
          NumAdd(i, j))))

    assertEquals(
      parse(phrase(pSetExpr), "if i < j then S[i] else F diff S[j]").runR(symTab + i + j),
      CondSetExpr(
        LT(i, j),
        SetRef(S, List(i)),
        Diff(
          SetRef(F),
          SetRef(S, List(j)))))

    /* TODO verify precedence of IndExpr `{X}' vs SetLit `{X}', with X being a Set in the first case and a Param in the second
        param nA;
        set A2 := A diff {nA}; # yields error because it enterpretes `{nA}' as an IndExprSet
     */

    assertEquals(
      parse(phrase(pSetExpr), "1..10 union 21..30").runR,
      Union(
        ArithSet(NumLit(1), NumLit(10)),
        ArithSet(NumLit(21), NumLit(30))))

    assertEquals(
      parse(phrase(pSetExpr), "(A union B) inter (I cross J)").runR,
      Inter(
        Union(SetRef(A), SetRef(B)),
        Cross(SetRef(I), SetRef(J))))

    assertEquals(
      parse(phrase(pSetExpr), "A union (B inter I) cross J").runR,
      Union(
        SetRef(A),
        Cross(
          Inter(
            SetRef(B),
            SetRef(I)),
          SetRef(J))))

    assertEquals(
      parse(phrase(pSetExpr), "A union B inter I cross J").runR,
      Union(
        SetRef(A),
        Inter(
          SetRef(B),
          Cross(
            SetRef(I),
            SetRef(J)))))

    assertEquals(
      parse(phrase(pSetExpr), "1..10 cross (if i < j then {'a', 'b', 'c'} else {'d', 'e', 'f'})").runR(symTab + i + j),
      Cross(
        ArithSet(NumLit(1), NumLit(10)),
        CondSetExpr(
          LT(i, j),
          SetLit(
            List(StringLit("a")),
            List(StringLit("b")),
            List(StringLit("c"))),
          SetLit(
            List(StringLit("d")),
            List(StringLit("e")),
            List(StringLit("f"))))))

    assertEquals(
      parse(phrase(pSetExpr), "1..10 cross if i < j then {'a', 'b', 'c'} else {'d', 'e', 'f'}").runR(symTab + i + j),
      Cross(
        ArithSet(NumLit(1), NumLit(10)),
        CondSetExpr(
          LT(i, j),
          SetLit(
            List(StringLit("a")),
            List(StringLit("b")),
            List(StringLit("c"))),
          SetLit(
            List(StringLit("d")),
            List(StringLit("e")),
            List(StringLit("f"))))))

    assertEquals(
      parse(phrase(pSetExpr), "A union B inter I cross J union B1 inter I1 cross J1").runR,
      Union(
        Union(
          SetRef(A),
          Inter(
            SetRef(B),
            Cross(
              SetRef(I),
              SetRef(J)))),
        Inter(
          SetRef(B1),
          Cross(
            SetRef(I1),
            SetRef(J1)))))

    assertEquals(
      parse(phrase(pSetExpr), "A union if i then I else J union B1 inter I1 cross J1").runR(symTab + i),
      Union(
        SetRef(A),
        CondSetExpr(
          i,
          SetRef(I),
          Union(
            SetRef(J),
            Inter(
              SetRef(B1),
              Cross(
                SetRef(I1),
                SetRef(J1)))))))

    assertEquals(
      parse(phrase(pSetExpr), "A union if i then I union B1 else I1 cross J1").runR(symTab + i),
      Union(
        SetRef(A),
        CondSetExpr(
          i,
          Union(
            SetRef(I),
            SetRef(B1)),
          Cross(
            SetRef(I1),
            SetRef(J1)))))

    assertEquals(
      parse(phrase(pLogicExpr), "i+1").runR(symTab + i),
      NumAdd(i, NumLit(1)))

    assertEquals(
      parse(phrase(pLogicExpr), "a[i,j] < 1.5").runR(symTab + i + j),
      LT(ParamRef(a, List(i, j)), NumLit(1.5)))

    assertEquals(
      parse(phrase(pLogicExpr), "x[i+1,j-1] <> 'Mar' & year").runR(symTab + i + j),
      NEq(
        ParamRef(
          x,
          List(
            NumAdd(i, NumLit(1)),
            NumSub(j, NumLit(1)))),
        Concat(StringLit("Mar"), ParamRef(year))))

    assertEquals(
      parse(phrase(pLogicExpr), "(i+1, 'Jan') not in I cross J").runR(symTab + i),
      NotIn(
        List(NumAdd(i, NumLit(1)), StringLit("Jan")),
        Cross(SetRef(I), SetRef(J))))

    assertEquals(
      parse(phrase(pLogicExpr), "S union T within A[i] inter B[j]").runR(symTab + i + j),
      Within(
        Union(
          SetRef(S),
          SetRef(T)),
        Inter(
          SetRef(A, List(i)),
          SetRef(B, List(j)))))

    assertEquals(
      parse(phrase(pLogicExpr), "forall{i in I, j in J} a[i,j] < .5 * b").runR,
      Forall(
        IndExpr(
          List(
            IndEntry(List(i), SetRef(I)),
            IndEntry(List(j), SetRef(J))),
          None),
        LT(
          ParamRef(a, List(i, j)),
          NumMult(NumLit(0.5), ParamRef(b)))))

    assertEquals(
      parse(phrase(pLogicExpr), "(a[i,j] < 1.5 or b[i] >= a[i,j])").runR(symTab + i + j),
      Disj(
        LT(ParamRef(a, List(i, j)), NumLit(1.5)),
        GTE(ParamRef(b, List(i)), ParamRef(a, List(i, j)))))

    assertEquals(
      parse(phrase(pLogicExpr), "not (a[i,j] < 1.5 or b[i] >= a[i,j]) and (i,j) in S").runR(symTab + i + j),
      Conj(
        Neg(Disj(
          LT(ParamRef(a, List(i, j)), NumLit(1.5)),
          GTE(ParamRef(b, List(i)), ParamRef(a, List(i, j))))),
        In(List(i, j), SetRef(S))))

    assertEquals(
      parse(phrase(pLogicExpr), "(i,j) in S or (i,j) not in T diff U").runR(symTab + i + j),
      Disj(
        In(
          List(i, j),
          SetRef(S)),
        NotIn(
          List(i, j),
          Diff(SetRef(T), SetRef(U)))))

    assertEquals(
      parse(phrase(pLogicExpr), "x < y").runR,
      LT(ParamRef(x), ParamRef(y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x <= y").runR,
      LTE(ParamRef(x), ParamRef(y)))
    assertEquals(
      parse(phrase(pLogicExpr), "x = y").runR,
      Eq(ParamRef(x), ParamRef(y)))
    assertEquals(
      parse(phrase(pLogicExpr), "x == y").runR,
      Eq(ParamRef(x), ParamRef(y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x > y").runR,
      GT(ParamRef(x), ParamRef(y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x >= y").runR,
      GTE(ParamRef(x), ParamRef(y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x <> y").runR,
      NEq(ParamRef(x), ParamRef(y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x != y").runR,
      NEq(ParamRef(x), ParamRef(y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x in Y").runR,
      In(List(ParamRef(x)), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "(x1, x2) in Y").runR(symTab + x1 + x2),
      In(List(x1, x2), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x not in Y").runR,
      NotIn(List(ParamRef(x)), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "x !in Y").runR,
      NotIn(List(ParamRef(x)), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "(x1, x2) not in Y").runR(symTab + x1 + x2),
      NotIn(List(x1, x2), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "(x1, x2) !in Y").runR(symTab + x1 + x2),
      NotIn(List(x1, x2), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "X within Y").runR,
      Within(SetRef(X), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "X not within Y").runR,
      NotWithin(SetRef(X), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "X !within Y").runR,
      NotWithin(SetRef(X), SetRef(Y)))

    assertEquals(
      parse(phrase(pLogicExpr), "forall{i in X} p[i]").runR,
      Forall(
        IndExpr(
          List(IndEntry(List(i), SetRef(X))),
          None),
        ParamRef(p, List(i))))

    assertEquals(
      parse(phrase(pLogicExpr), "exists{i in X} p[i]").runR,
      Exists(
        IndExpr(
          List(IndEntry(List(i), SetRef(X))),
          None),
        ParamRef(p, List(i))))

    assertEquals(
      parse(phrase(pLogicExpr), "forall{i in X} p[i] and exists{j in Y} q[j] or a > b ").runR,
      Forall(
        IndExpr(
          List(IndEntry(List(i), SetRef(X))),
          None),
        Conj(
          ParamRef(p, List(i)),
          Disj(
            Exists(
              IndExpr(
                List(IndEntry(List(j), SetRef(Y))),
                None),
              ParamRef(q, List(j))),
            GT(ParamRef(a), ParamRef(b))))))

    assertEquals(
      parse(phrase(pLogicExpr), "forall{i in X} p[i] or exists{j in Y} q[j] and a > b ").runR,
      Disj(
        Forall(
          IndExpr(
            List(IndEntry(List(i), SetRef(X))),
            None),
          ParamRef(p, List(i))),
        Exists(
          IndExpr(
            List(IndEntry(List(j), SetRef(Y))),
            None),
          Conj(ParamRef(q, List(j)), GT(ParamRef(a), ParamRef(b))))))

    assertEquals(
      parse(phrase(pLogicExpr), "not x or !x || x and y or x && y").runR,
      Disj(
        Disj(
          Disj(
            Neg(ParamRef(x)),
            Neg(ParamRef(x))),
          Conj(
            ParamRef(x),
            ParamRef(y))),
        Conj(
          ParamRef(x),
          ParamRef(y))))

    assertEquals(
      parse(phrase(pLinExpr), "z").runR,
      VarRef(z))

    assertEquals(
      parse(phrase(pLinExpr), "z[i,j]").runR(symTab + i + j),
      VarRef(z, List(i, j)))

    assertEquals(
      parse(phrase(pLinExpr), "x[i,j]").runR(symTab + i + j),
      ParamRef(x, List(i, j)))

    assertEquals(
      parse(phrase(pLinExpr), "x * z").runR,
      LinMult(ParamRef(x), VarRef(z)))

    assertEquals(
      parse(phrase(pLinExpr), "z * x").runR,
      LinMult(ParamRef(x), VarRef(z)))

    assertEquals(
      parse(phrase(pLinExpr), "x * y").runR,
      NumMult(ParamRef(x), ParamRef(y)))

    assertTrue(
      parse(phrase(pLinExpr), "z * w").isTypeMismatch)

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (a[i] * z[i,j] + 3 * w)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          LinMult(
            NumLit(3),
            VarRef(w)))))

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (z[i,j] * a[i] + w * 3)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          LinMult(
            NumLit(3),
            VarRef(w)))))

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (z[i,j] * a[i] + 3 * 3 * p)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          NumMult(
            NumMult(
              NumLit(3),
              NumLit(3)),
            ParamRef(p)))))

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (z[i,j] * a[i] + 3 * 3 ** p)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          NumMult(
            NumLit(3),
            NumRaise(
              NumLit(3),
              ParamRef(p))))))

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (z[i,j] * a[i] + 3 * 3 / p * w)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          LinMult(
            NumDiv(
              NumMult(
                NumLit(3),
                NumLit(3)),
              ParamRef(p)),
            VarRef(w)))))

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (z[i,j] * a[i] + w * 3 * 3 / p)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          LinDiv(
            LinMult(
              NumLit(3),
              LinMult(
                NumLit(3),
                VarRef(w))),
            ParamRef(p)))))

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (z[i,j] * a[i] +  3 * w / 3 * p)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          LinMult(
            ParamRef(p),
            LinDiv(
              LinMult(
                NumLit(3),
                VarRef(w)),
              NumLit(3))))))

    assertEquals(
      parse(phrase(pLinExpr), "sum{j in J} (z[i,j] * a[i] +  (2 ** 3) ** 4 ** 5)").runR(symTab + i),
      LinSum(
        IndExpr(List(IndEntry(List(j), SetRef(J)))),
        LinAdd(
          LinMult(
            ParamRef(a, List(i)),
            VarRef(z, List(i, j))),
          NumRaise(
            NumRaise(NumLit(2), NumLit(3)),
            NumRaise(NumLit(4), NumLit(5))))))

    assertEquals(
      parse(phrase(pLinExpr), "if i in I then x[i,j] else 1.5 * z + 3").runR(symTab + i + j),
      CondLinExpr(
        In(
          List(i),
          SetRef(I)),
        ParamRef(x, List(i, j)),
        LinAdd(
          LinMult(
            NumLit(1.5),
            VarRef(z)),
          NumLit(3)).some))

    assertEquals(
      parse(phrase(pLinExpr), "(a[i,j] * z[i,j] + w[i-1] + .1)").runR(symTab + i + j),
      LinAdd(
        LinAdd(
          LinMult(
            ParamRef(a, List(i, j)),
            VarRef(z, List(i, j))),
          VarRef(w, List(NumSub(i, NumLit(1))))),
        NumLit(0.1)))

    assertEquals(
      parse(phrase(pSetStat), """set G;""").runR,
      SetStat("G"))

    assertEquals(
      parse(phrase(pSetStat), """set G within V cross V;""").runR,
      SetStat("G", atts = List(SetWithin(Cross(SetRef(V), SetRef(V))))))

    /* Recursive set definitions are not supported
    assertEquals(
      parse(phrase(pSetStat), """set step{s in 1..maxiter} dimen 2 := if s = 1 then E else step[s-1] union setof{k in V, (i,k) in step[s-1], (k,j) in step[s-1]}(i,j);""")
        .runR,
      SetStat(
        name = "step",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(
                  List(s),
                  ArithSet(NumLit(1), ParamRef(maxiter)))))),
        atts =
          List(
            SetDimen(NumLit(2)),
            SetAssign(
              CondSetExpr(
                Eq(s, NumLit(1)),
                SetRef(E),
                Union(
                  SetRef(step, List(NumSub(s, NumLit(1)))),
                  SetOf(
                    IndExpr(
                      List(
                        IndEntry(
                          List(k),
                          SetRef(V)),
                        IndEntry(
                          List(i, k),
                          SetRef(step, List(NumSub(s, NumLit(1))))),
                        IndEntry(
                          List(k, j),
                          SetRef(step, List(NumSub(s, NumLit(1))))))),
                    List(i, j))))))))
*/

    assertEquals(
      parse(phrase(pSetStat), """set G{i in I, j in J}, within B[i+1] cross C[j-1], within D diff E, default {('abc',123), (321,'cba')};""").runR,
      SetStat(
        name = "G",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(i), SetRef(I)),
                IndEntry(List(j), SetRef(J))))),
        atts = List(
          SetWithin(
            Cross(
              SetRef(B, List(NumAdd(i, NumLit(1)))),
              SetRef(C, List(NumSub(j, NumLit(1)))))),
          SetWithin(
            Diff(
              SetRef(D),
              SetRef(E))),
          SetDefault(
            SetLit(
              List(StringLit("abc"), NumLit(123)),
              List(NumLit(321), StringLit("cba")))))))

    assertEquals(
      parse(phrase(pParamStat), """param P;""").runR,
      ParamStat("P"))

    assertEquals(
      parse(phrase(pParamStat), """param units{raw, prd} >= 0;""").runR,
      ParamStat(
        name = "units",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(), SetRef(raw)),
                IndEntry(List(), SetRef(prd))))),
        atts =
          List(
            ParamGTE(NumLit(0)))))

    assertEquals(
      parse(phrase(pParamStat), """param profit{prd, 1..M+1};""").runR,
      ParamStat(
        name = "profit",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(), SetRef(prd)),
                IndEntry(List(), ArithSet(NumLit(1), NumAdd(ParamRef(M), NumLit(1)))))))))

    assertEquals(
      parse(phrase(pParamStat), """param L := 20, integer, >= 0, <= 100;""").runR,
      ParamStat(
        name = "L",
        atts =
          List(
            ParamAssign(NumLit(20)),
            Integer,
            ParamGTE(NumLit(0)),
            ParamLTE(NumLit(100)))))

    /* Recursive parameter definitions not supported  
    assertEquals(
      parse(phrase(pParamStat), """param comb 'n choose k' {n in 0..N, k in 0..n} := if k = 0 or k = n then 1 else comb[n-1,k-1] + comb[n-1,k];""").runR,
      ParamStat(
        name = "comb",
        alias = Some(StringLit("n choose k")),
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(n), ArithSet(NumLit(0), ParamRef(N))),
                IndEntry(List(k), ArithSet(NumLit(0), n))))),
        atts =
          List(
            ParamAssign(
              CondNumExpr(
                Disj(Eq(k, NumLit(0)), Eq(k, n)),
                NumLit(1),
                NumAdd(
                  ParamRef(comb, List(NumSub(n, NumLit(1)), NumSub(k, NumLit(1)))),
                  ParamRef(comb, List(NumSub(n, NumLit(1)), k))).some)))))
   */

    assertEquals(
      parse(phrase(pParamStat), """param p2{i in I, j in J}, integer, >= 0, <= i+j, in A[i] symdiff B[j], in C[i,j], default 0.5 * (i + j);""").runR,
      ParamStat(
        name = "p2",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(i), SetRef(I)),
                IndEntry(List(j), SetRef(J))))),
        atts =
          List(
            Integer,
            ParamGTE(NumLit(0)),
            ParamLTE(NumAdd(i, j)),
            ParamIn(
              SymDiff(
                SetRef(A, List(i)),
                SetRef(B, List(j)))),
            ParamIn(
              SetRef(C, List(i, j))),
            ParamDefault(NumMult(NumLit(0.5), NumAdd(i, j))))))

    assertEquals(
      parse(phrase(pParamStat), """param month "el mes" symbolic default 'May' in {'Mar', 'Apr', 'May'};""").runR,
      ParamStat(
        name = "month",
        alias = Some(StringLit("el mes")),
        atts =
          List(
            Symbolic,
            ParamDefault(StringLit("May")),
            ParamIn(
              SetLit(
                List(StringLit("Mar")),
                List(StringLit("Apr")),
                List(StringLit("May")))))))

    assertEquals(
      parse(phrase(pVarStat), """var xv >= 0;""").runR,
      VarStat(
        name = "xv",
        atts =
          List(
            VarGTE(NumLit(0)))))

    assertEquals(
      parse(phrase(pVarStat), """var yv{I,J};""").runR,
      VarStat(
        name = "yv",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(), SetRef(I)),
                IndEntry(List(), SetRef(J)))))))

    assertEquals(
      parse(phrase(pVarStat), """var make{i in prd}, integer, >= commit[i], <= market[i];""").runR,
      VarStat(
        name = "make",
        domain =
          Some(IndExpr(List(IndEntry(List(i), SetRef(prd))))),
        atts =
          List(
            Integer,
            VarGTE(ParamRef(commit, List(i))),
            VarLTE(ParamRef(market, List(i))))))

    assertEquals(
      parse(phrase(pVarStat), """var store{raw, 1..M+1} >= 0;""").runR,
      VarStat(
        name = "store",
        domain =
          Some(
            IndExpr(List(
              IndEntry(List(), SetRef(raw)),
              IndEntry(List(), ArithSet(NumLit(1), NumAdd(ParamRef(M), NumLit(1))))))),
        atts =
          List(VarGTE(NumLit(0)))))

    assertEquals(
      parse(phrase(pVarStat), """var zv{i in I, j in J} >= i+j;""").runR,
      VarStat(
        name = "zv",
        domain =
          Some(
            IndExpr(List(
              IndEntry(List(i), SetRef(I)),
              IndEntry(List(j), SetRef(J))))),
        atts =
          List(VarGTE(NumAdd(i, j)))))

    // TODO make this actual tests

    val make = VarStat("make")
    val store = VarStat("store")
    val rprd = VarStat("rprd")
    val dpp = VarStat("dpp")
    val max_prd = ParamStat("max_prd")
    val units = ParamStat("units")
    val pt = ParamStat("pt")
    val crews = ParamStat("crews")
    val profit = ParamStat("profit")

    parse(phrase(pConstraintStat), """s.t. r: 1, >= x + y + z, >= 0;""").runR

    parse(phrase(pConstraintStat), """limit{t in T}: sum{j in prd} make[j,t] <= max_prd;""").runR(symTab + make + max_prd)

    parse(phrase(pConstraintStat), """subject to balance{i in raw, t in T}: store[i,t+1] = store[i,t] - sum{j in prd} units[i,j] * make[j,t];""").runR(symTab + store + make + units)

    parse(phrase(pConstraintStat), """subject to rlim 'regular-time limit' {t in T}: sum{k in prd} pt[k] * rprd[k,t] >= 1.3 * dpp[t] * crews[t];""").runR(symTab + rprd + dpp + pt + crews)

    parse(phrase(pObjectiveStat), """minimize obj: x + 1.5 * (y + z);""").runR

    parse(phrase(pObjectiveStat), """maximize total_profit: sum{k in prd} profit[k] * make[k];""").runR(symTab + make + profit)

    assertEquals(
      parse(phrase(pConstraintStat),
        """s.t. cap{s in S, t in T, i in C}: a[s, t, i] - b[s, t, i] <= z[s, t, i] <= p[s, t, i] + q[s, t, i];""").runR,
      DLTEConstraintStat("cap",
        domain = IndExpr(List(
          IndEntry(List(s), SetRef(S)),
          IndEntry(List(t), SetRef(T)),
          IndEntry(List(i), SetRef(C)))).some,
        lower = NumSub(
          ParamRef(a, List(s, t, i)),
          ParamRef(b, List(s, t, i))),
        expr = VarRef(z, List(s, t, i)),
        upper = NumAdd(
          ParamRef(p, List(s, t, i)),
          ParamRef(q, List(s, t, i)))))

    assertEquals(
      parse(phrase(pModel), """set G; param pB; var xC; minimize D: 2*xC; s.t. E: 1 = 1; end;""").runR,
      Model(List(
        SetStat("G"),
        ParamStat("pB"),
        VarStat("xC"),
        Minimize("D", expr = LinMult(NumLit(2), VarRef(VarStat("xC")))),
        EqConstraintStat("E", left = NumLit(1), right = NumLit(1)))))

    assertEquals(
      parse(phrase(pModel), """
		  set G;
		  param pB;
		  var xC;
		  maximize D: 2*xC;
		  subject to E: 1 = pB;
		  end;""").runR,
      Model(List(
        SetStat("G"),
        ParamStat("pB"),
        VarStat("xC"),
        Maximize("D", expr = LinMult(NumLit(2), VarRef(VarStat("xC")))),
        EqConstraintStat("E", left = NumLit(1), right = ParamRef(ParamStat("pB"))))))


    // XXX add tests for sample models
    /*
    import amphip.model.show._
    import scalax.io.Resource
    
    object data {
      val modelA = Resource.fromClasspath("examples.modelA").string

    }

    val start = System.currentTimeMillis()
    val res = parseModel(data.modelA).runR(SymTab.empty)
    val end = System.currentTimeMillis()
    val elapsed = end - start
    println(s"$elapsed millis")
    assertTrue(res.isInstanceOf[Model])

    val model = res.asInstanceOf[Model]
    assertEquals(parse(phrase(pModel), model.shows).runR(SymTab.empty), model)
    * 
    */

  }
}

