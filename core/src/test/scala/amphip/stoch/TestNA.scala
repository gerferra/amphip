package amphip.stoch

import org.junit.Assert._
import org.junit.Test

import amphip.dsl._

class TestNA {

  @Test
  def testAssignIndicesBasic(): Unit = {
    val t = dummy
    val s = dummy
    val S = set 
    val T = set

    val I  = set
    
    val x1  = xvar(T,S)
    val (entries1, tA1, sA1) = nonanticipativity.assignIndices(x1.domain.get.entries, T, S, t, s)
    assertEquals(List(t,s), entries1.flatMap(_.indices))
    assertEquals(tA1, t)
    assertEquals(sA1, s)
    
    val i_ = dummy("i_", synthetic = true)
    val x2a = xvar(T,S,I)
    val (entries2a, tA2a, sA2a) = nonanticipativity.assignIndices(x2a.domain.get.entries, T, S, t, s)
    assertEquals(List(t,s,i_), entries2a.flatMap(_.indices))
    assertEquals(tA2a, t)
    assertEquals(sA2a, s)
    
    val i  = dummy
    val x2b = xvar(T, S, i in I)
    val (entries2b, tA2b, sA2b) = nonanticipativity.assignIndices(x2b.domain.get.entries, T, S, t, s)
    assertEquals(List(t,s,i), entries2b.flatMap(_.indices))
    assertEquals(tA2b, t)
    assertEquals(sA2b, s)

    val i1_ = dummy("i1_", synthetic = true)
    val x3 = xvar(T, S, I * I)
    val (entries3, tA3, sA3) = nonanticipativity.assignIndices(x3.domain.get.entries, T, S, t, s)
    assertEquals(List(t, s, i_, i1_), entries3.flatMap(_.indices))
    assertEquals(tA3, t)
    assertEquals(sA3, s)

    val i2_ = dummy("i2_", synthetic = true)
    val x4 = xvar(T, S, I * I, I)
    val (entries4, tA4, sA4) = nonanticipativity.assignIndices(x4.domain.get.entries, T, S, t, s)
    assertEquals(List(t, s, i_, i1_, i2_), entries4.flatMap(_.indices))
    assertEquals(tA4, t)
    assertEquals(sA4, s)
  }

  @Test
  def testAssignIndicesExplicit(): Unit = {
    val t = dummy
    val s = dummy
    val S = set 
    val T = set

    val i1 = dummy
    val i2 = dummy

    val x1  = xvar(i1 in T, i2 in S)
    val (entries1, tA1, sA1) = nonanticipativity.assignIndices(x1.domain.get.entries, T, S, t, s)
    assertEquals(List(i1,i2), entries1.flatMap(_.indices))
    assertEquals(tA1, i1)
    assertEquals(sA1, i2)

    val x2a = xvar(i1 in T, S)
    val (entries2a, tA2a, sA2a) = nonanticipativity.assignIndices(x2a.domain.get.entries, T, S, t, s)
    assertEquals(List(i1,s), entries2a.flatMap(_.indices))
    assertEquals(tA2a, i1)
    assertEquals(sA2a, s)

    val x2b = xvar(T, i2 in S)
    val (entries2b, tA2b, sA2b) = nonanticipativity.assignIndices(x2b.domain.get.entries, T, S, t, s)
    assertEquals(List(t,i2), entries2b.flatMap(_.indices))
    assertEquals(tA2b, t)
    assertEquals(sA2b, i2)
  }

  @Test
  def testAssignIndicesOverlapping(): Unit = {
    val t_ = dummy("t_", synthetic = true)
    val s_ = dummy("s_", synthetic = true)
    val t  = dummy
    val s  = dummy
    
    val S = set 
    val T = set
    val I  = set 

    val x1  = xvar(T, S, t in I)
    val (entries1, tA1, sA1) = nonanticipativity.assignIndices(x1.domain.get.entries, T, S, t, s)
    assertEquals(List(t_, s, t), entries1.flatMap(_.indices))
    assertEquals(tA1, t_)
    assertEquals(sA1, s)

    val x2  = xvar(T, S, s in I)
    val (entries2, tA2, sA2) = nonanticipativity.assignIndices(x2.domain.get.entries, T, S, t, s)
    assertEquals(List(t, s_, s), entries2.flatMap(_.indices))
    assertEquals(tA2, t)
    assertEquals(sA2, s_)

    val x3  = xvar(T, S, t in I, s in I)
    val (entries3, tA3, sA3) = nonanticipativity.assignIndices(x3.domain.get.entries, T, S, t, s)
    assertEquals(List(t_, s_, t, s), entries3.flatMap(_.indices))
    assertEquals(tA3, t_)
    assertEquals(sA3, s_)
  }

  @Test
  def testUniqueDummy(): Unit = {
    val t = dummy
    val s = dummy
    val S = set 
    val T = set

    val x1   = xvar(t in T, s in S)
    val s1   = dummy("s1", synthetic = true)
    val s2   = dummy("s2", synthetic = true)
    val x1s1 = nonanticipativity.uniqueDummy(x1.domain.get.entries.flatMap(_.indices), s, 1)
    val x1s2 = nonanticipativity.uniqueDummy(x1.domain.get.entries.flatMap(_.indices), s, 2)
    assertEquals(List(s1, s2), List(x1s1, x1s2))

    val x2   = xvar(s1 in T, s in S)
    val s11  = dummy("s11", synthetic = true)
    val x2s1 = nonanticipativity.uniqueDummy(x2.domain.get.entries.flatMap(_.indices), s, 1)
    val x2s2 = nonanticipativity.uniqueDummy(x2.domain.get.entries.flatMap(_.indices), s, 2)
    assertEquals(List(s11, s2), List(x2s1, x2s2))

    val I = set
    val s1a  = dummy
    val x3   = xvar(s1 in T, s in S, s1a in I, s2 in I)
    val s1a1 = dummy("s1a1", synthetic = true)
    val s22  = dummy("s22" , synthetic = true)
    val x3s1 = nonanticipativity.uniqueDummy(x3.domain.get.entries.flatMap(_.indices), s, 1)
    val x3s2 = nonanticipativity.uniqueDummy(x3.domain.get.entries.flatMap(_.indices), s, 2)
    assertEquals(List(s1a1, s22), List(x3s1, x3s2))

  }
}