package amphip.model

import cats.syntax.show._
import cats.syntax.option._

import org.junit.Assert._
import org.junit.Test

import amphip.model.ast._
import amphip.model.dsl._

class TestShow {

  @Test
  def test(): Unit = {

    assertEquals("1", (NumLit(1): NumExpr).show)
    assertEquals(""""abc"""", (StringLit("abc"): SymExpr).show)
    assertEquals(""""That's all"""", (StringLit("That's all"): SymExpr).show)
    assertEquals(""""She said: ""No"""""", (StringLit("""She said: "No""""): SymExpr).show)

    val A = set("A")
    val B = set("B")
    val C = set("C")
    val D = set("D")
    val S = set("S")
    val T = set("T")
    val J = set("J")
    val I = set("I")
    val E = set("E")
    val F = set("F")
    val U = set("U")
    val V = set("V")
    val step = set("step", ind(S))

    val raw = set("raw")
    val prd = set("prd")
    val N = param("N")
    val a = param("a", ind(A, B, C))

    val b = param("b", ind(B, C))

    val c = param("c", ind(C))

    val alpha = param("alpha", ind(I))

    val comb = param("comb", ind(A, B))

    val q = param("q", ind(I))

    val name = param("name", ind(I))

    val city = param("city", ind(I))
    val test = param("test", ind(I))
    val maxiter = param("maxiter")

    val i = dummy("i")
    val j = dummy("j")
    val k = dummy("k")
    val l = dummy("l")
    val n = dummy("n")
    val s = dummy("s")
    val s1 = dummy("s1")
    val s2 = dummy("s2")
    val t = dummy("t")
    val t1 = dummy("t1")
    val t2 = dummy("t2")
    val _x1 = dummy(gen.dummy.freshName, synthetic = true)
    val _x2 = dummy(gen.dummy.freshName, synthetic = true)

    val p = param("p", ind(S, T))
    val units = param("units", ind(S, T))
    val x = xvar("x", ind(I))
    val y = xvar("y", ind(I))
    val z = xvar("z", ind(I))
    val commit = param("commit", ind(I))
    val market = param("market", ind(I))
    val max_prd = xvar("max_prd", ind(I))
    val make = xvar("make", ind(I))
    val store = xvar("store", ind(I))

    assertEquals(a("May 2003", j, 1),
      ParamRef(a, List(StringLit("May 2003"), j, NumLit(1))))

    assertEquals("""a["May 2003", j, 1]""", a("May 2003", j, 1).show)
    assertEquals("abs(-3)", (Abs(-3): NumFuncRef).show)
    assertEquals("round(4.5)", (Round(4.5): NumFuncRef).show)
    assertEquals("round(4.5, 1)", (Round(4.5, NumLit(1).some): NumFuncRef).show)
    assertEquals("Uniform01()", (Uniform01(): NumFuncRef).show)

    assertEquals(sum(i in S)(sum(j in J)(i * j)),
      NumSum(
        IndExpr(List(IndEntry(List(i), SetRef(S))), None),
        NumSum(
          IndExpr(List(IndEntry(List(j), SetRef(J))), None),
          NumMult(i, j))))

    assertEquals("(sum{i in S} (sum{j in J} (i * j)))", sum(i in S)(sum(j in J)(i * j)).show)

    assertEquals((3 * sum(i in S)(sum(j in J)(i * j))) * 5,
      NumMult(
        NumMult(
          NumLit(3),
          NumSum(IndExpr(List(IndEntry(List(i), SetRef(S))), None),
            NumSum(IndExpr(List(IndEntry(List(j), SetRef(J))), None),
              NumMult(i, j)))),
        NumLit(5)))

    assertEquals("((3 * (sum{i in S} (sum{j in J} (i * j)))) * 5)", ((3 * sum(i in S)(sum(j in J)(i * j))) * 5).show)

    assertEquals(NumLit(7) + sum(s in S)(p(s, t)) + 8,
      NumAdd(
        NumAdd(
          NumLit(7),
          NumSum(
            IndExpr(List(IndEntry(List(s), S)), None),
            p(s, t))),
        NumLit(8)))

    assertEquals("((7 + (sum{s in S} p[s, t])) + 8)", (7 + sum(s in S)(p(s, t)) + 8).show)

    assertEquals(-sum(i in I)(i),
      NumUnaryMinus(NumSum(IndExpr(List(IndEntry(List(i), I))), i)))

    assertEquals("""(-(sum{i in I} i))""", (-sum(i in I)(i)).show)

    assertEquals(prod(i in S)(alpha(i)),
      NumProd(IndExpr(List(IndEntry(List(i), S))), alpha(i)))

    assertEquals("(prod{i in S} alpha[i])", prod(i in S)(alpha(i)).show)

    assertEquals(min(i in S)(alpha(i)),
      NumMin(IndExpr(List(IndEntry(List(i), S))), alpha(i)))

    assertEquals("(min{i in S} alpha[i])", min(i in S)(alpha(i)).show)

    assertEquals(max(i in S)(alpha(i)),
      NumMax(IndExpr(List(IndEntry(List(i), S))), alpha(i)))

    assertEquals("(max{i in S} alpha[i])", max(i in S)(alpha(i)).show)

    assertEquals(xif(i in I) { 2 } { q(i) },
      CondNumExpr(
        In(List(i), I),
        NumLit(2),
        q(i).some))

    assertEquals("(if (i in I) then 2 else q[i])", xif(i in I) { 2 } { q(i) }.show)

    assertEquals(xif1(i in I) { 2 }, CondNumExpr(In(List(i), I), NumLit(2)))

    assertEquals("(if (i in I) then 2)", xif1(i in I) { 2 }.show)

    assertEquals((2 ** (3 ** (4 ** 5))), NumRaise(NumLit(2), NumRaise(NumLit(3), NumRaise(NumLit(4), NumLit(5)))))

    assertEquals("(2 ** (3 ** (4 ** 5)))", (2 ** (3 ** (4 ** 5))).show)

    assertEquals("(((2 ** 3) ** 4) ** 5)", (2 ** 3 ** 4 ** 5).show)

    assertEquals(-q, NumUnaryMinus(q))

    assertEquals("(-q)", (-q).show)

    assertEquals(+q, NumUnaryPlus(q))

    assertEquals("(+q)", (+q).show)

    assertEquals(+a * -c, NumMult(
      NumUnaryPlus(a),
      NumUnaryMinus(c)))

    assertEquals("((+a) * (-c))", (+a * -c).show)

    assertEquals(+a / -c, NumDiv(
      NumUnaryPlus(a),
      NumUnaryMinus(c)))

    assertEquals("((+a) / (-c))", (+a / -c).show)

    assertEquals(+a /~ -c, NumDivExact(
      NumUnaryPlus(a),
      NumUnaryMinus(c)))

    assertEquals("((+a) div (-c))", (+a /~ -c).show)

    assertEquals(+a % -c, NumMod(
      NumUnaryPlus(a),
      NumUnaryMinus(c)))

    assertEquals("((+a) mod (-c))", (+a % -c).show)

    assertEquals(7 * ((2 ** 3) ** (4 ** 5)) * -a, NumMult(NumMult(NumLit(7), NumRaise(NumRaise(NumLit(2), NumLit(3)), NumRaise(NumLit(4), NumLit(5)))), NumUnaryMinus(a)))

    assertEquals("((7 * ((2 ** 3) ** (4 ** 5))) * (-a))", (7 * ((2 ** 3) ** (4 ** 5)) * -a).show)

    assertEquals((num(1) * 2 / 3 /~ 4) % 5, NumMod(NumDivExact(NumDiv(NumMult(NumLit(1), NumLit(2)), NumLit(3)), NumLit(4)), NumLit(5)))

    assertEquals("((((1 * 2) / 3) div 4) mod 5)", ((num(1) * 2 / 3 /~ 4) % 5).show)

    assertEquals((num(1) % 2 /~ 3) / 4 * 5, NumMult(NumDiv(NumDivExact(NumMod(NumLit(1), NumLit(2)), NumLit(3)), NumLit(4)), NumLit(5)))

    assertEquals("((((1 mod 2) div 3) / 4) * 5)", ((num(1) % 2 /~ 3) / 4 * 5).show)

    assertEquals(+a + -c, NumAdd(
      NumUnaryPlus(a),
      NumUnaryMinus(c)))

    assertEquals("((+a) + (-c))", (+a + -c).show)

    assertEquals(+a - -c, NumSub(
      NumUnaryPlus(a),
      NumUnaryMinus(c)))

    assertEquals("((+a) - (-c))", (+a - -c).show)

    assertEquals(+a less -c, NumLess(
      NumUnaryPlus(a),
      NumUnaryMinus(c)))

    assertEquals("((+a) less (-c))", (+a less -c).show)

    assertEquals(b(i, j) + .5 * c, NumAdd(b(i, j), NumMult(NumLit(0.5), c)))

    assertEquals("(b[i, j] + (0.5 * c))", (b(i, j) + .5 * c).show)

    assertEquals("min(1, 2, 3)", (Min(1, 2, 3): NumFuncRef).show)


    assertEquals(i in I, IndEntry(List(i), I))
    assertEquals(3 in I, In(List(num(3)), I))

    assertEquals(
      xif(-a > 2) {
        sum { i in I }(a(i)) + 8
      } {
        max { i in I }(a(i)) + 9
      },
      CondNumExpr(
        GT(NumUnaryMinus(a), NumLit(2)),
        NumAdd(
          NumSum(IndExpr(List(IndEntry(List(i), I))), a(i)),
          NumLit(8)),
        NumAdd(
          NumMax(IndExpr(List(IndEntry(List(i), I))), a(i)),
          NumLit(9)).some))

    assertEquals("(if ((-a) > 2) then ((sum{i in I} a[i]) + 8) else ((max{i in I} a[i]) + 9))",
      xif(-a > 2) { sum(i in I)(a(i)) + 8 } { max(i in I)(a(i)) + 9 }.show)

    assertEquals("(if ((-a) > 2) then (if ((+b) < -8) then 10 else 20) else (if ((+b) >= -8) then 30 else 40))",
      (CondNumExpr(
        GT(NumUnaryMinus(a), NumLit(2)),
        CondNumExpr(
          LT(NumUnaryPlus(b), NumLit(-8)),
          NumLit(10),
          NumLit(20).some),
        CondNumExpr(
          GTE(NumUnaryPlus(b), NumLit(-8)),
          NumLit(30),
          NumLit(40).some).some): NumExpr).show)

    assertEquals("(if ((-a) > 2) then (if ((+b) < -8) then 10) else 20)",
      (CondNumExpr(
        GT(NumUnaryMinus(a), NumLit(2)),
        CondNumExpr(
          LT(NumUnaryPlus(b), NumLit(-8)),
          NumLit(10)),
        NumLit(20).some): NumExpr).show)

    assertEquals("(if ((-a) > 2) then (if ((+b) < -8) then 10 else 20))",
      (CondNumExpr(
        GT(NumUnaryMinus(a), NumLit(2)),
        CondNumExpr(
          LT(NumUnaryPlus(b), NumLit(-8)),
          NumLit(10),
          NumLit(20).some)): NumExpr).show)

    assertEquals("substr(name[i], (k + 1), 3)",
      (Substr(name(i), NumAdd(k, 1), NumLit(3).some): SymExpr).show)

    assertEquals("substr(name[i], (k + 1))",
      (Substr(name(i), NumAdd(k, 1)): SymExpr).show)

    assertEquals("""time2str(123, "format")""",
      (Time2str(123, "format"): SymExpr).show)

    assertEquals("""(((("abc[" & i) & ",") & j) & "]")""",
      (Concat(
        Concat(
          Concat(
            Concat(StringLit("abc["), i),
            StringLit(",")),
          j),
        StringLit("]")): SymExpr).show)

    assertEquals("""((("from " & city[i]) & " to ") & city[j])""",
      (Concat(
        Concat(
          Concat(StringLit("from "), city(i)),
          StringLit(" to ")),
        city(j)): SymExpr).show)

    assertEquals("(if (i in I) then a[i, j] else b[(i + 1)])",
      (CondSymExpr(
        In(List(i), I),
        a(i, j),
        ParamRef(b, List(NumAdd(i, 1))).some): SymExpr).show)

    assertEquals("""("begin" & (if ((-a) > 2) then (("middle-end" & test) & (if ((-a) <= 2) then ("-end" & test) else "?"))))""",
      (Concat(
        StringLit("begin"),
        CondSymExpr(
          GT(NumUnaryMinus(a), NumLit(2)),
          Concat(
            Concat(StringLit("middle-end"), test),
            CondSymExpr(
              LTE(NumUnaryMinus(a), NumLit(2)),
              Concat(StringLit("-end"), test),
              StringLit("?").some)))): SymExpr).show)

    assertEquals("""(("begin" & (8 + (if ((-a) > 2) then 16))) & (if ((-a) <= 2) then 8))""",
      (Concat(
        Concat(
          StringLit("begin"),
          SymNumExpr(
            NumAdd(
              NumLit(8),
              CondNumExpr(
                GT(NumUnaryMinus(a), NumLit(2)),
                NumLit(16))))),
        SymNumExpr(
          CondNumExpr(
            LTE(NumUnaryMinus(a), NumLit(2)),
            NumLit(8)))): SymExpr).show)

    assertEquals("""((10 * b[i, j]) & ".bis")""",
      (Concat(
        SymNumExpr(NumMult(NumLit(10), b(i, j))),
        StringLit(".bis")): SymExpr).show)

    assertEquals("s in S",
      IndEntry(List(s), S).show)

    assertEquals("s in S : (s > 3)",
      IndEntry(List(s), S, Some(GT(s,NumLit(3)))).show)
    
    assertEquals("{s in S, t in T}",
      IndExpr(
        List(
          IndEntry(List(s), S),
          IndEntry(List(t), T))).show)

    assertEquals("{(s1, s2) in S, t in T}",
      IndExpr(
        List(
          IndEntry(List(s1, s2), S),
          IndEntry(List(t), T))).show)

    assertEquals("{S, t in T}",
      IndExpr(
        List(
          IndEntry(Nil, S),
          IndEntry(List(t), T))).show)

    assertEquals("{s in S, (t1, t2) in T}",
      IndExpr(
        List(
          IndEntry(List(s), S),
          IndEntry(List(t1, t2), T))).show)

    assertEquals("{s in S, T}",
      IndExpr(
        List(
          IndEntry(List(s), S),
          IndEntry(Nil, T))).show)

    assertEquals("{s in S, T}",
      IndExpr(
        List(
          IndEntry(List(s), S),
          IndEntry(Nil, T))).show)

    assertEquals("{i in A, (j, k) in B, l in C}",
      IndExpr(
        List(
          IndEntry(List(i), A),
          IndEntry(List(j, k), B),
          IndEntry(List(l), C))).show)

    assertEquals("{A, B, C}",
      IndExpr(
        List(
          IndEntry(List(), A),
          IndEntry(List(), B),
          IndEntry(List(), C))).show)

    assertEquals("{i in A, ((i - 1), k) in B, l in C}",
      IndExpr(
        List(
          IndEntry(List(i), A),
          IndEntry(List(_x1, k), B, Eq(_x1, NumSub(i, NumLit(1))).some), //  `$x1 == i-1` because it's an expression
          IndEntry(List(l), C))).show)

    assertEquals("{i in A, (i, k) in B, l in C}",
      IndExpr(
        List(
          IndEntry(List(i), A),
          IndEntry(List(_x1, k), B, Eq(_x1, i).some), // `$x1 == i` because `i` already declared
          IndEntry(List(l), C))).show)

    assertEquals("{i in A, l in B, ((i - 1), l) in C}",
      IndExpr(
        List(
          IndEntry(List(i), A),
          IndEntry(List(l), B),
          IndEntry(List(_x1, _x2), C, Conj(Eq(_x1, NumSub(i, NumLit(1))), Eq(_x2, l)).some) //  `$x1 == i-1 && $x2 == l`
          )).show)

    assertEquals("""{i in A, (j, k) in B, l in C : ((i <= 5) and (k <> "Mar"))}""",
      IndExpr(
        List(
          IndEntry(List(i), A),
          IndEntry(List(j, k), B),
          IndEntry(List(l), C)),
        Conj(LTE(i, NumLit(5)), NEq(k, StringLit("Mar"))).some).show)

    assertEquals("""{(123, "aa"), (i, "bb"), ((j - 1), "cc")}""",
      (SetLit(
        List(NumLit(123), StringLit("aa")),
        List(i, StringLit("bb")),
        List(NumSub(j, NumLit(1)), StringLit("cc"))): SetExpr).show)

    assertEquals("{123, i, (j - 1)}",
      (SetLit(
        List(NumLit(123)),
        List(i),
        List(NumSub(j, NumLit(1)))): SetExpr).show)

    assertEquals("{}", (SetLit(): SetExpr).show)

    assertEquals("I", (I: SetExpr).show)

    assertEquals("S[(i - 1), (j + 1)]",
      (S(NumSub(i, NumLit(1)),
        NumAdd(j, NumLit(1))): SetExpr).show)

    assertEquals("1 .. (t - 1) by 2",
      (ArithSet(
        NumLit(1),
        NumSub(t, NumLit(1)),
        Some(NumLit(2))): SetExpr).show)

    assertEquals("(setof{i in I, j in J} ((i + 1), (j - 1)))",
      (SetOf(
        IndExpr(
          List(
            IndEntry(List(i), I),
            IndEntry(List(j), J))),
        List(
          NumAdd(i, NumLit(1)),
          NumSub(j, NumLit(1)))): SetExpr).show)

    assertEquals("(setof{i in I, j in J} (i + j))",
      (SetOf(
        IndExpr(
          List(
            IndEntry(List(i), I),
            IndEntry(List(j), J))),
        List(
          NumAdd(i, j))): SetExpr).show)

    assertEquals("(if (i < j) then S[i] else (F diff S[j]))",
      (CondSetExpr(
        LT(i, j),
        S(i),
        Diff(F, S(j))): SetExpr).show)

    assertEquals("(1 .. 10 union 21 .. 30)",
      (Union(
        ArithSet(NumLit(1), NumLit(10)),
        ArithSet(NumLit(21), NumLit(30))): SetExpr).show)

    assertEquals("((A union B) inter (I cross J))",
      (Inter(
        Union(A, B),
        Cross(I, J)): SetExpr).show)

    assertEquals("""(1 .. 10 cross (if (i < j) then {"a", "b", "c"} else {"d", "e", "f"}))""",
      (Cross(
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
            List(StringLit("f"))))): SetExpr).show)

    assertEquals("""(((i + 1), "Jan") not in (I cross J))""",
      (NotIn(
        List(NumAdd(i, NumLit(1)), StringLit("Jan")),
        Cross(I, J)): LogicExpr).show)

    assertEquals("((S union T) within (A[i] inter B[j]))",
      (Within(
        Union(
          S,
          T),
        Inter(
          A(i),
          B(j))): LogicExpr).show)

    assertEquals("(forall{i in I, j in J} (a[i, j] < (0.5 * b)))",
      (Forall(
        IndExpr(
          List(
            IndEntry(List(i), I),
            IndEntry(List(j), J))),
        LT(
          a(i, j),
          NumMult(NumLit(0.5), b))): LogicExpr).show)

    assertEquals("((a[i, j] < 1.5) or (b[i] >= a[i, j]))",
      (Disj(
        LT(a(i, j), NumLit(1.5)),
        GTE(b(i), a(i, j))): LogicExpr).show)

    assertEquals("((not ((a[i, j] < 1.5) or (b[i] >= a[i, j]))) and ((i, j) in S))",
      (Conj(
        Neg(Disj(
          LT(a(i, j), NumLit(1.5)),
          GTE(b(i), a(i, j)))),
        In(List(i, j), S)): LogicExpr).show)

    assertEquals("(((i, j) in S) or ((i, j) not in (T diff U)))",
      (Disj(
        In(
          List(i, j),
          S),
        NotIn(
          List(i, j),
          Diff(T, U))): LogicExpr).show)

    assertEquals("(a == b)",
      (Eq(a, b): LogicExpr).show)

    assertEquals("(forall{t in T} (p[t] and ((exists{i in I} q[i]) or (a > b))))",
      (Forall(
        IndExpr(
          List(IndEntry(List(t), T)),
          None),
        Conj(
          p(t),
          Disj(
            Exists(
              IndExpr(List(IndEntry(List(i), I))),
              q(i)),
            GT(a, b)))): LogicExpr).show)

    assertEquals("""set V;""",
      SetStat("V").show)

    assertEquals("""set E within (V cross V);""",
      SetStat("E", atts = List(SetWithin(Cross(V, V)))).show)

    assertEquals("""set step{s in 1 .. maxiter} dimen 2, := (if (s == 1) then E else (step[(s - 1)] union (setof{k in V, (i, k) in step[(s - 1)], (k, j) in step[(s - 1)]} (i, j))));""",
      SetStat(
        name = "step",
        domain =
          IndExpr(
            List(
              IndEntry(
                List(s),
                ArithSet(NumLit(1), maxiter)))).some,
        atts =
          List(
            SetDimen(2),
            SetAssign(
              CondSetExpr(
                Eq(s, NumLit(1)),
                E,
                Union(
                  step(NumSub(s, NumLit(1))),
                  SetOf(
                    IndExpr(
                      List(
                        IndEntry(List(k), V),
                        IndEntry(List(i, k), step(NumSub(s, NumLit(1)))),
                        IndEntry(List(k, j), step(NumSub(s, NumLit(1)))))),
                    List(i, j))))))).show)

    assertEquals("""set A{i in I, j in J} within (B[(i + 1)] cross C[(j - 1)]), within (D diff E), default {("abc", 123), (321, "cba")};""",
      SetStat(
        name = "A",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(i), I),
                IndEntry(List(j), J)))),
        atts = List(
          SetWithin(
            Cross(
              B(NumAdd(i, NumLit(1))),
              C(NumSub(j, NumLit(1))))),
          SetWithin(
            Diff(
              D,
              E)),
          SetDefault(
            SetLit(
              List(StringLit("abc"), NumLit(123)),
              List(NumLit(321), StringLit("cba")))))).show)

    assertEquals("""param P;""", ParamStat("P").show)

    assertEquals("""param units{raw, prd} >= 0;""",
      ParamStat(
        name = "units",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(), raw),
                IndEntry(List(), prd)))),
        atts =
          List(
            ParamGTE(NumLit(0)))).show)

    assertEquals("""param profit{prd, 1 .. (N + 1)};""",
      ParamStat(
        name = "profit",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(), prd),
                IndEntry(
                  List(),
                  ArithSet(NumLit(1), NumAdd(N, NumLit(1)))))))).show)

    assertEquals("""param N := 20, integer, >= 0, <= 100;""",
      ParamStat(
        name = "N",
        atts =
          List(
            ParamAssign(NumLit(20)),
            Integer,
            ParamGTE(NumLit(0)),
            ParamLTE(NumLit(100)))).show)

    assertEquals("""param comb "n choose k" {n in 0 .. N, k in 0 .. n} := (if ((k == 0) or (k == n)) then 1 else (comb[(n - 1), (k - 1)] + comb[(n - 1), k]));""",
      ParamStat(
        name = "comb",
        alias = Some(StringLit("n choose k")),
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(n), ArithSet(NumLit(0), N)),
                IndEntry(List(k), ArithSet(NumLit(0), n))))),
        atts =
          List(
            ParamAssign(
              CondNumExpr(
                Disj(Eq(k, NumLit(0)), Eq(k, n)),
                NumLit(1),
                NumAdd(
                  comb(NumSub(n, NumLit(1)), NumSub(k, NumLit(1))),
                  comb(NumSub(n, NumLit(1)), k)).some)))).show)

    assertEquals("""param p{i in I, j in J} integer, >= 0, <= (i + j), in (A[i] symdiff B[j]), in C[i, j], default (0.5 * (i + j));""",
      ParamStat(
        name = "p",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(i), I),
                IndEntry(List(j), J)))),
        atts =
          List(
            Integer,
            ParamGTE(NumLit(0)),
            ParamLTE(NumAdd(i, j)),
            ParamIn(
              SymDiff(
                A(i),
                B(j))),
            ParamIn(
              C(i, j)),
            ParamDefault(NumMult(NumLit(0.5), NumAdd(i, j))))).show)

    assertEquals("""param month "el mes" symbolic, default "May", in {"Mar", "Apr", "May"};""",
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
                List(StringLit("May")))))).show)

    assertEquals("""var x >= 0;""",
      VarStat(
        name = "x",
        atts =
          List(
            VarGTE(NumLit(0)))).show)

    assertEquals("""var y{I, J};""",
      VarStat(
        name = "y",
        domain =
          Some(
            IndExpr(
              List(
                IndEntry(List(), I),
                IndEntry(List(), J))))).show)

    assertEquals("""var make{i in prd} integer, >= commit[i], <= market[i];""",
      VarStat(
        name = "make",
        domain =
          Some(IndExpr(List(IndEntry(List(i), prd)))),
        atts =
          List(
            Integer,
            VarGTE(commit(i)),
            VarLTE(market(i)))).show)

    assertEquals("""var store{raw, 1 .. (N + 1)} >= 0;""",
      VarStat(
        name = "store",
        domain =
          Some(
            IndExpr(List(
              IndEntry(List(), raw),
              IndEntry(List(), ArithSet(NumLit(1), NumAdd(N, NumLit(1))))))),
        atts =
          List(VarGTE(NumLit(0)))).show)

    assertEquals("""var z{i in I, j in J} >= (i + j);""",
      VarStat(
        name = "z",
        domain =
          Some(
            IndExpr(List(
              IndEntry(List(i), I),
              IndEntry(List(j), J)))),
        atts =
          List(VarGTE(NumAdd(i, j)))).show)

    assertEquals("""s.t. r: 1 >= ((x + y) + z) >= 0;""",
      (DGTEConstraintStat(
        name = "r",
        upper = NumLit(1),
        expr = LinAdd(LinAdd(x, y), z),
        lower = NumLit(0)): ConstraintStat).show)

    assertEquals("""s.t. limit{t in 1 .. N}: (sum{j in prd} make[j, t]) <= max_prd;""",
      (LTEConstraintStat(
        name = "limit",
        domain = IndExpr(List(IndEntry(List(t), ArithSet(1, N)))).some,
        left = LinSum(IndExpr(List(IndEntry(List(j), prd))), make(j, t)),
        right = max_prd): ConstraintStat).show)

    assertEquals("""s.t. balance{i in raw, t in 1 .. N}: store[i, (t + 1)] == (store[i, t] - (sum{j in prd} (units[i, j] * make[j, t])));""",
      (EqConstraintStat(
        name = "balance",
        domain =
          IndExpr(
            List(
              IndEntry(List(i), raw),
              IndEntry(List(t), ArithSet(1, N)))).some,
        left = store(i, NumAdd(t, 1)),
        right =
          LinSub(
            store(i, t),
            LinSum(
              IndExpr(List(IndEntry(List(j), prd))),
              LinMult(
                units(i, j),
                make(j, t))))): ConstraintStat).show)

    assertEquals("""s.t. rlim "regular-time limit" {t in T}: (sum{j in prd} (a[p] * x[p, t])) >= ((1.3 * b[t]) * y[t]);""",
      (GTEConstraintStat(
        name = "rlim",
        alias = StringLit("regular-time limit").some,
        domain = IndExpr(List(IndEntry(List(t), T))).some,
        left = LinSum(
          IndExpr(List(IndEntry(List(j), prd))),
          LinMult(
            a(p),
            x(p, t))),
        right = LinMult(
          NumMult(1.3, b(t)),
          y(t))): ConstraintStat).show)

    assertEquals("""minimize obj: (x + (1.5 * (y + z)));""",
      (Minimize("obj", expr = LinAdd(x, LinMult(1.5, LinAdd(y, z)))): ObjectiveStat).show)

    assertEquals("""maximize total_profit: (sum{i in prd} (a[i] * x[i]));""",
      (Maximize("total_profit", expr = LinSum(IndExpr(List(IndEntry(List(i), prd))), LinMult(a(i), x(i)))): ObjectiveStat).show)

    assertEquals("""set A;
  							 |
								 |param B;
								 |
      					 |var C;
      					 |
      					 |minimize D: (2 * x);
      					 |
      					 |s.t. E: 1 == 1;
      					 |
      					 |end;""".stripMargin,
      Model(List(
        SetStat("A"),
        ParamStat("B"),
        VarStat("C"),
        Minimize("D", expr = LinMult(NumLit(2), x)),
        EqConstraintStat("E", left = NumLit(1), right = NumLit(1)))).show)

  }
}
