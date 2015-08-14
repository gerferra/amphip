package amphip.model.toSyntax

object TestToSyntax {

  import syntax._
  import instances._

  val one = 1
  val oneL = 1L
  val oneF = 1f
  val oneD = 1d
  val two = num(2)

  one to two
  oneL to two
  oneF to two
  oneD to two

  1 to 2

  two to one
  two to oneL
  two to oneF
  two to oneD

  two to two
}

