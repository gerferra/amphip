package amphip.data

import cats.syntax.option._

import org.junit.Assert._
import org.junit.Test

import amphip.base._
import amphip.dsl._
import amphip.model.ast.{ ConstraintStat, DLTEConstraintStat, DGTEConstraintStat, ObjectiveStat, Minimize, Maximize }
import amphip.data.ModelData._

class TestEval {

  @Test
  def test(): Unit = {

    implicit val modelData = ModelData()

    // ASSIGN AND DEFAULT

    val p = param("p")
    val pDef = p default 2
    val pAssg = pDef := 3

    val S = set("S")
    val SDef = S default List(2, 3, 4)
    val SAssg = SDef := List(3, 4, 5)

    val key = DataKey

    assertEquals(eval(p(), modelData.plusParam(key("p"), 1)), 1: SimpleData)
    assertEquals(eval(pDef()), 2: SimpleData)
    assertEquals(eval(pDef(), modelData.plusParam(key("p"), 1)), 1: SimpleData)
    assertEquals(eval(pAssg()), 3: SimpleData)
    assertEquals(eval(pAssg(), modelData.plusParam(key("p"), 1)), 3: SimpleData)

    assertEquals(eval(S(), modelData.plusSet(key("S"), List(1, 2, 3))), List[SetTuple](1, 2, 3))
    assertEquals(eval(SDef()), List[SetTuple](2, 3, 4))
    assertEquals(eval(SDef(), modelData.plusSet(key("S"), List(1, 2, 3))), List[SetTuple](1, 2, 3))
    assertEquals(eval(SAssg()), List[SetTuple](3, 4, 5))
    assertEquals(eval(SAssg(), modelData.plusSet(key("S"), List(1, 2, 3))), List[SetTuple](3, 4, 5))

    // default sets

    val Empty = set("Empty") default Nil
    val IndexedEmpty = set("IndexedEmpty", ind(SDef)) default Nil

    assertEquals(eval(Empty()), Nil)
    assertEquals(eval(IndexedEmpty(2)), Nil)
    assertEquals(eval(IndexedEmpty(3)), Nil)
    assertEquals(eval(IndexedEmpty(4)), Nil)

    ////

    // STATEMENTS

    // aux
    val I = set("I") := List(3, 4, 5)
    val i = dummy("i")
    val J = set("J") := List(6, 7, 8)
    val j = dummy("j")
    val A = set("A") := List(1, 2)
    val B = set("B") := List(3, 4)
    val a = param("a") := 3
    val b = param("b") := 2
    val x = xvar("x") >= 0
    val y = xvar("y") >= 0

    val IInd = set("I", I)
    val AInd = set("A", I)
    val BInd = set("B", I)
    val aInd = param("a", I)
    val bInd = param("b", I)
    val xInd = xvar("x", I) >= 0
    val yInd = xvar("y", I) >= 0

    // unindexed statements

    val s1 = set("s1").dimen(2).within(I).:=(A).default(B)
    val p1 = param("p1").integer.binary.symbolic.>(a).>=(b).===(a + b).=!=(a * b).<(a + a).<=(a * a).in(I).:=(b * b).default(b + b)
    val x1 = xvar("x1").integer.binary.>=(b).===(a + b).<=(a * a)
    val objMax = maximize("objMax") { a * x + b * y }
    val objMin = minimize("objMin") { a * x + b * y }
    val ctrEq = st("ctrEq") { a * x === b * y }
    val ctrLTE = st("ctrLTE") { a * x <= b * y }
    val ctrGTE = st("ctrGTE") { a * x >= b * y }
    //val ctrDLTE = st("ctrDLTE") { a  <= b * y <= b } // XXX this syntax could be supported 
    val ctrDLTE: ConstraintStat = DLTEConstraintStat("ctrDLTE", lower = a, expr = b * y, upper = b)
    val ctrDGTE: ConstraintStat = DGTEConstraintStat("ctrDGTE", upper = a, expr = b * y, lower = b)

    assertEquals(eval(s1), LinkedMap(key("s1") -> set("s1").dimen(2).within(List(3, 4, 5)).:=(List(1, 2)).default(List(3, 4))))
    assertEquals(eval(p1), LinkedMap(key("p1") -> param("p1").integer.binary.symbolic.>(3).>=(2).===(5).=!=(6).<(6).<=(9).in(List(3, 4, 5)).:=(4).default(4)))
    assertEquals(eval(x1), LinkedMap(key("x1") -> xvar("x1").integer.binary.>=(2).===(5).<=(9)))
    assertEquals(eval(objMax), LinkedMap(key("objMax") -> maximize("objMax") { 3 * x + 2 * y }))
    assertEquals(eval(objMin), LinkedMap(key("objMin") -> minimize("objMin") { 3 * x + 2 * y }))

    /* this crashes the compiler:
     * `LinkedMap(ctrEq.name -> 3 * x === 2 * y)'
     * it shouldn't compile because of the missing parenthesis around "3*x===2*y"
     * but it shouldn't crash the compiler... 
     * */
    assertEquals(eval(ctrEq), LinkedMap(key("ctrEq") -> st("ctrEq") { 3 * x === 2 * y }))
    assertEquals(eval(ctrLTE), LinkedMap(key("ctrLTE") -> st("ctrLTE") { 3 * x <= 2 * y }))
    assertEquals(eval(ctrGTE), LinkedMap(key("ctrGTE") -> st("ctrGTE") { 3 * x >= 2 * y }))
    assertEquals(eval(ctrDLTE), LinkedMap(key("ctrDLTE") -> DLTEConstraintStat("ctrDLTE", lower = 3, expr = 2 * y, upper = 2)))
    assertEquals(eval(ctrDGTE), LinkedMap(key("ctrDGTE") -> DGTEConstraintStat("ctrDGTE", upper = 3, expr = 2 * y, lower = 2)))

    // indexed statements

    val s1Ind = set("s1", i in I).dimen(3).within(IInd(i)).:=(AInd(i)).default(BInd(i))
    val p1Ind = param("p1", i in I)
      .integer
      .binary
      .symbolic
      .>(aInd(i)).>=(bInd(i))
      .===(aInd(i) + bInd(i)).=!=(aInd(i) * bInd(i))
      .<(aInd(i) + aInd(i)).<=(aInd(i) * aInd(i))
      .in(IInd(i))
      .:=(bInd(i) * bInd(i)).default(bInd(i) + bInd(i))
    val x1Ind = xvar("x1", i in I).integer.binary.>=(bInd(i)).===(aInd(i) + bInd(i)).<=(aInd(i) * aInd(i))
    /* MathProg permits indexed objectives, but they end converted to constraints, so the DSL don't allow them */
    val objMaxInd: ObjectiveStat = Maximize("objMax", domain = ind(i in I).some, expr = aInd(i) * xInd(i) + bInd(i) * yInd(i))
    val objMinInd: ObjectiveStat = Minimize("objMin", domain = ind(i in I).some, expr = aInd(i) * xInd(i) + bInd(i) * yInd(i))
    val ctrEqInd = st("ctrEq", i in I) { aInd(i) * xInd(i) === bInd(i) * yInd(i) }
    val ctrLTEInd = st("ctrLTE", i in I) { aInd(i) * xInd(i) <= bInd(i) * yInd(i) }
    val ctrGTEInd = st("ctrGTE", i in I) { aInd(i) * xInd(i) >= bInd(i) * yInd(i) }
    val ctrDLTEInd: ConstraintStat = DLTEConstraintStat("ctrDLTE", domain = ind(i in I).some, lower = aInd(i), expr = bInd(i) * yInd(i), upper = bInd(i))
    val ctrDGTEInd: ConstraintStat = DGTEConstraintStat("ctrDGTE", domain = ind(i in I).some, upper = aInd(i), expr = bInd(i) * yInd(i), lower = bInd(i))

    val indParamData =
      LinkedMap[DataKey, SimpleData](
        key("a", 3) -> 6,
        key("a", 4) -> 7,
        key("a", 5) -> 8,
        key("b", 3) -> 9,
        key("b", 4) -> 10,
        key("b", 5) -> 11)

    val indSetData =
      LinkedMap[DataKey, SetData](
        key("I", 3) -> List(3, 4, 5),
        key("I", 4) -> List(4, 5, 6),
        key("I", 5) -> List(5, 6, 7),
        key("A", 3) -> List(1, 2),
        key("A", 4) -> List(2, 3),
        key("A", 5) -> List(3, 4),
        key("B", 3) -> List(3, 4),
        key("B", 4) -> List(4, 5),
        key("B", 5) -> List(5, 6))

    val indModelData = modelData.plusParams(indParamData).plusSets(indSetData)

    assertEquals(eval(s1Ind, indModelData),
      LinkedMap(
        key("s1", 3) -> set("s1").dimen(3).within(List(3, 4, 5)).:=(List(1, 2)).default(List(3, 4)),
        key("s1", 4) -> set("s1").dimen(3).within(List(4, 5, 6)).:=(List(2, 3)).default(List(4, 5)),
        key("s1", 5) -> set("s1").dimen(3).within(List(5, 6, 7)).:=(List(3, 4)).default(List(5, 6))))
    assertEquals(eval(p1Ind, indModelData),
      LinkedMap(
        key("p1", 3) -> param("p1").integer.binary.symbolic.>(6).>=(9).===(15).=!=(54).<(12).<=(36).in(List(3, 4, 5)).:=(81).default(18),
        key("p1", 4) -> param("p1").integer.binary.symbolic.>(7).>=(10).===(17).=!=(70).<(14).<=(49).in(List(4, 5, 6)).:=(100).default(20),
        key("p1", 5) -> param("p1").integer.binary.symbolic.>(8).>=(11).===(19).=!=(88).<(16).<=(64).in(List(5, 6, 7)).:=(121).default(22)))
    assertEquals(eval(x1Ind, indModelData),
      LinkedMap(
        key("x1", 3) -> xvar("x1").integer.binary.>=(9).===(15).<=(36),
        key("x1", 4) -> xvar("x1").integer.binary.>=(10).===(17).<=(49),
        key("x1", 5) -> xvar("x1").integer.binary.>=(11).===(19).<=(64)))
    assertEquals(eval(objMaxInd, indModelData),
      LinkedMap(
        key("objMax", 3) -> maximize("objMax") { 6 * xInd(3) + 9 * yInd(3) },
        key("objMax", 4) -> maximize("objMax") { 7 * xInd(4) + 10 * yInd(4) },
        key("objMax", 5) -> maximize("objMax") { 8 * xInd(5) + 11 * yInd(5) }))
    assertEquals(eval(objMinInd, indModelData),
      LinkedMap(
        key("objMin", 3) -> minimize("objMin") { 6 * xInd(3) + 9 * yInd(3) },
        key("objMin", 4) -> minimize("objMin") { 7 * xInd(4) + 10 * yInd(4) },
        key("objMin", 5) -> minimize("objMin") { 8 * xInd(5) + 11 * yInd(5) }))

    assertEquals(eval(ctrEqInd, indModelData),
      LinkedMap(
        key("ctrEq", 3) -> st("ctrEq") { 6 * xInd(3) === 9 * yInd(3) },
        key("ctrEq", 4) -> st("ctrEq") { 7 * xInd(4) === 10 * yInd(4) },
        key("ctrEq", 5) -> st("ctrEq") { 8 * xInd(5) === 11 * yInd(5) }))
    assertEquals(eval(ctrLTEInd, indModelData),
      LinkedMap(
        key("ctrLTE", 3) -> st("ctrLTE") { 6 * xInd(3) <= 9 * yInd(3) },
        key("ctrLTE", 4) -> st("ctrLTE") { 7 * xInd(4) <= 10 * yInd(4) },
        key("ctrLTE", 5) -> st("ctrLTE") { 8 * xInd(5) <= 11 * yInd(5) }))
    assertEquals(eval(ctrGTEInd, indModelData),
      LinkedMap(
        key("ctrGTE", 3) -> st("ctrGTE") { 6 * xInd(3) >= 9 * yInd(3) },
        key("ctrGTE", 4) -> st("ctrGTE") { 7 * xInd(4) >= 10 * yInd(4) },
        key("ctrGTE", 5) -> st("ctrGTE") { 8 * xInd(5) >= 11 * yInd(5) }))
    assertEquals(eval(ctrDLTEInd, indModelData),
      LinkedMap(
        key("ctrDLTE", 3) -> DLTEConstraintStat("ctrDLTE", lower = 6, expr = 9 * yInd(3), upper = 9),
        key("ctrDLTE", 4) -> DLTEConstraintStat("ctrDLTE", lower = 7, expr = 10 * yInd(4), upper = 10),
        key("ctrDLTE", 5) -> DLTEConstraintStat("ctrDLTE", lower = 8, expr = 11 * yInd(5), upper = 11)))
    assertEquals(eval(ctrDGTEInd, indModelData),
      LinkedMap(
        key("ctrDGTE", 3) -> DGTEConstraintStat("ctrDGTE", upper = 6, expr = 9 * yInd(3), lower = 9),
        key("ctrDGTE", 4) -> DGTEConstraintStat("ctrDGTE", upper = 7, expr = 10 * yInd(4), lower = 10),
        key("ctrDGTE", 5) -> DGTEConstraintStat("ctrDGTE", upper = 8, expr = 11 * yInd(5), lower = 11)))

    ////

    // INDEXING EXPRESSIONS

    val varX = xvar("varX", ind(i in I, j in J) | (j - i === 1))
    assertEquals(eval(varX, modelData), LinkedMap(key("varX", 5, 6) -> xvar("varX")))

    val i1 = dummy("i1")
    val i2 = dummy("i2")
    val varY = xvar("varY", ind(i1 in I, i2 in I) | (bInd(i2) - aInd(i1) === 1))
    assertEquals(eval(varY, modelData.plusParams(indParamData)), LinkedMap(key("varY", 5, 3) -> xvar("varY")))

    {
      val AData = List(4,7,9)
      val BData = List((1,"Jan"), (1,"Feb"), (2,"Mar"), (2,"Apr"), (3,"May"), (3,"Jun"))
      val CData = List("a","b","c")

      val i = dummy
      val A = set := AData
      val j = dummy 
      val k = dummy
      val B = set := BData
      val l = dummy
      val C = set := CData
      
      {
        // {i in A, (j,k) in B, l in C}
        val result =
          for {
            i     <- AData 
            (j,k) <- BData 
            l     <- CData
          } yield {
            LinkedMap(
              key("i") -> (i: SimpleData), 
              key("j") -> (j: SimpleData), 
              key("k") -> (k: SimpleData), 
              key("l") -> (l: SimpleData))
          }

        val evResult = eval(ind(i in A, (j,k) in B, l in C), ModelData())
        assertEquals(result, evResult)
      }
      
      {
        // {i in A, (i-1,k) in B, l in C}
        val result =
          for {
            i     <- AData 
            (j,k) <- BData if j == i-1
            l     <- CData
          } yield {
            LinkedMap(
              key("i") -> (i: SimpleData), 
              key("k") -> (k: SimpleData), 
              key("l") -> (l: SimpleData))
          }

        val evResult = eval(ind(i in A, (j,k).in(B) | j === i-1, l in C), ModelData())
        assertEquals(result, evResult)
      }
    }

    {
      import amphip.model.ast.ParamStat 
      val N = param := 10
      val n = dummy 
      lazy val fib: ParamStat = {
        param(n in (0 to N)) :=
          xif (n === 0) { 
            0 
          } { 
            xif (n === 1) {
              1
            } {
              fib(n-1) + fib(n-2) 
            }
          }
      }

      import spire.math.{fib => _fib}
      val result =
        for {
          n     <- 0 to 10
        } yield {
          key("fib", n) -> (param("fib") := _fib(n.toLong))
        }
      val resMap = LinkedMap(result: _*)

      val evResult = eval(fib, ModelData())
      assertEquals(resMap, evResult)
    }

    {
      import amphip.model.ast.ParamStat 
      val N = param := 5
      val n = dummy 
      lazy val f: ParamStat = {
        param(n in (0 to N)) :=
          xif (n === N) { 
            3 
          } { 
            1 + f(n + 1)
          }
      }
      val resMap =
        LinkedMap(
          key("f", 0) -> (param("f") := 8),
          key("f", 1) -> (param("f") := 7),
          key("f", 2) -> (param("f") := 6),
          key("f", 3) -> (param("f") := 5),
          key("f", 4) -> (param("f") := 4),
          key("f", 5) -> (param("f") := 3)
        )

      val evResult = eval(f, ModelData())
      assertEquals(resMap, evResult)
    }
  }
}