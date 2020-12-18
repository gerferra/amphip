# amphip
`amphip` is a collection of experiments around working with GNU MathProg in Scala in high level, aiming to support scenario-based multistage stochastic programming models.

<img style="float:right" alt="Hyalella azteca" src="https://upload.wikimedia.org/wikipedia/commons/5/59/Hyalella_azteca.jpg" width="200" />

It includes:
* A deep embedding of the MathProg language in Scala
  * The model is represented with an encoding of the language AST using case classes and sealed traits
  * The syntax is mimicked using smart constructors and combinators
* A functional parser of the MathProg language using the `scala-parser-combinators` library
* Pretty-printing of the AST that generates the corresponding MathProg code
* An evaluator of arbitrary MathProg expressions in the context of some model and data instance
* An extension to the embedding to support scenario-based multistage stochastic programming

I talked about the techniques used to encode MathProg syntax in this library at [Scala by the Bay 2015](http://scalabythebay2015.sched.org/event/8cbbc52523ac9e5a905361e00d357099)
* [Video](https://www.youtube.com/watch?v=Od_AH-_XoEQ)
* [Slides](http://www.slideshare.net/gerferra/an-embedded-dsl-to-manipulate-mathprog-mixed-integer-programming-models-within-scala)

// TBC
