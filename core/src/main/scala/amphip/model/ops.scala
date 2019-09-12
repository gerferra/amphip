package amphip.model

import scala.annotation.implicitNotFound

object ops {

  @implicitNotFound("Ref is not defined for ${A}")
  trait RefOp[A, B, C] {
    def apply(a: => A, expr: List[B]): C
  }

  @implicitNotFound("=== is not defined for ${A}, ${B}")
  trait EqOp[A, B, C] {
    def eq(lhe: A, rhe: B): C
  }

  @implicitNotFound("=!= is not defined for ${A}, ${B}")
  trait NEqOp[A, B, C] {
    def neq(lhe: A, rhe: B): C
  }

  @implicitNotFound("< is not defined for ${A}, ${B}")
  trait LTOp[A, B, C] {
    def lt(lhe: A, rhe: B): C
  }

  @implicitNotFound("<= is not defined for ${A}, ${B}")
  trait LTEOp[A, B, C] {
    def lte(lhe: A, rhe: B): C
  }

  @implicitNotFound("> is not defined for ${A}, ${B}")
  trait GTOp[A, B, C] {
    def gt(lhe: A, rhe: B): C
  }

  @implicitNotFound(">= is not defined for ${A}, ${B}")
  trait GTEOp[A, B, C] {
    def gte(lhe: A, rhe: B): C
  }

  @implicitNotFound("Dimen is not defined for ${A}")
  trait DimenOp[A, B] {
    def dimen(a: A, n: Int): B
  }

  @implicitNotFound("Within is not defined for ${A}, ${B}")
  trait WithinOp[A, B, C] {
    def within(lhe: A, rhe: B): C
  }

  @implicitNotFound("Assign is not defined for ${A}, ${B}")
  trait AssignOp[A, B, C] {
    def assign(lhe: A, rhe: B): C
  }

  @implicitNotFound("Default is not defined for ${A}, ${B}")
  trait DefaultOp[A, B, C] {
    def default(lhe: A, rhe: B): C
  }

  @implicitNotFound("In is not defined for ${A}, ${B}")
  trait InOp[A, B, C] {
    def in(lhe: A, rhe: B): C
  }

  @implicitNotFound("${A} can't be integer")
  trait IntegerOp[A] {
    def integer(a: A): A
  }

  @implicitNotFound("${A} can't be binary")
  trait BinaryOp[A] {
    def binary(a: A): A
  }

  @implicitNotFound("${A} can't be symbolic")
  trait SymbolicOp[A] {
    def symbolic(a: A): A
  }

  @implicitNotFound("Cond is not defined for ${A}, ${B}, ${C}")
  trait CondOp[A, B, C, D] {
    def cond(test: A)(ifTrue: B)(otherwise: C): D
  }

  @implicitNotFound("Cond1 is not defined for ${A}, ${B}")
  trait Cond1Op[A, B, C] {
    def cond1(test: A)(ifTrue: B): C
  }

  @implicitNotFound("Addition is not defined for ${A}, ${B}")
  trait AddOp[A, B, C] {
    def add(lhe: A, rhe: B): C
  }

  @implicitNotFound("Substraction is not defined for ${A}, ${B}")
  trait SubOp[A, B, C] {
    def sub(lhe: A, rhe: B): C
  }

  @implicitNotFound("Less is not defined for ${A}, ${B}")
  trait LessOp[A, B, C] {
    def less(lhe: A, rhe: B): C
  }

  @implicitNotFound("Sum is not defined for ${A}, ${B}")
  trait SumOp[A, B, C] {
    def sum(indexing: A, integrand: B): C
  }

  @implicitNotFound("Prod is not defined for ${A}, ${B}")
  trait ProdOp[A, B, C] {
    def prod(indexing: A, integrand: B): C
  }

  @implicitNotFound("Max is not defined for ${A}, ${B}")
  trait MaxOp[A, B, C] {
    def max(indexing: A, integrand: B): C
  }

  @implicitNotFound("Min is not defined for ${A}, ${B}")
  trait MinOp[A, B, C] {
    def min(indexing: A, integrand: B): C
  }

  @implicitNotFound("Multiplication is not defined for ${A}, ${B}")
  trait MultOp[A, B, C] {
    def mult(lhe: A, rhe: B): C
  }

  @implicitNotFound("Division is not defined for ${A}, ${B}")
  trait DivOp[A, B, C] {
    def div(lhe: A, rhe: B): C
  }

  @implicitNotFound("Exact division is not defined for ${A}, ${B}")
  trait DivExactOp[A, B, C] {
    def divExact(lhe: A, rhe: B): C
  }

  @implicitNotFound("Modulus is not defined for ${A}, ${B}")
  trait ModOp[A, B, C] {
    def mod(lhe: A, rhe: B): C
  }

  @implicitNotFound("Unary plus is not defined for ${A}")
  trait UnaryPlusOp[A, B] {
    def plus(a: A): B
  }

  @implicitNotFound("Unary minus is not defined for ${A}")
  trait UnaryMinusOp[A, B] {
    def minus(a: A): B
  }

  @implicitNotFound("Raise is not defined for ${A}, ${B}")
  trait RaiseOp[A, B, C] {
    def raise(lhe: A, rhe: B): C
  }

  @implicitNotFound("Pipe is not defined for ${A}, ${B}")
  trait PipeOp[A, B, C] {
    def pipe(lhe: A, rhe: B): C
  }

  @implicitNotFound("Diff is not defined for ${A}, ${B}")
  trait DiffOp[A, B, C] {
    def diff(lhe: A, rhe: B): C
  }

  @implicitNotFound("SymDiff is not defined for ${A}, ${B}")
  trait SymDiffOp[A, B, C] {
    def symDiff(lhe: A, rhe: B): C
  }

  @implicitNotFound("Inter is not defined for ${A}, ${B}")
  trait InterOp[A, B, C] {
    def inter(lhe: A, rhe: B): C
  }

  @implicitNotFound("SetOf is not defined for ${A}, ${B}")
  trait SetOfOp[A, B, C] {
    def setOf(indexing: A, integrand: B*): C
  }

  @implicitNotFound("To is not defined for ${A}, ${B}")
  trait ToOp[A, B, C] {
    def to(t0: A, tf: B): C
  }

  @implicitNotFound("By is not defined for ${A}, ${B}")
  trait ByOp[A, B, C] {
    def by(exp: A, deltaT: B): C
  }

  @implicitNotFound("Disj is not defined for ${A}, ${B}")
  trait DisjOp[A, B, C] {
    def disj(lhe: A, rhe: B): C
  }

  @implicitNotFound("Forall is not defined for ${A}, ${B}")
  trait ForallOp[A, B, C] {
    def forall(indexing: A, integrand: B): C
  }

  @implicitNotFound("Exists is not defined for ${A}, ${B}")
  trait ExistsOp[A, B, C] {
    def exists(indexing: A, integrand: B): C
  }

  @implicitNotFound("Conj is not defined for ${A}, ${B}")
  trait ConjOp[A, B, C] {
    def conj(lhe: A, rhe: B): C
  }

  @implicitNotFound("Negation is not defined for ${A}")
  trait NegOp[A, B] {
    def not(a: A): B
  }

  //// FUNCTIONS

  @implicitNotFound("Size is not defined for ${A}")
  trait SizeOp[A, B] {
    def size(a: A): B
  }

}