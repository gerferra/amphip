# amphip
`amphip` is a collection of experiments around working with GNU MathProg in Scala, at a high level.

It includes:
* A deep embedding of the MathProg language in Scala
  * The model is represented with an encoding of the language AST using case classes and sealed traits
  * The syntax is mimicked using smart constructors and combinators
* A functional parser of the MathProg language using the `scala-parser-combinators` library
* Pretty-printing of the AST that generates the corresponding MathProg code
* An evaluator of arbitrary MathProg expressions in the context of some model and data instance

// TBC
