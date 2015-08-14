// GENERAL

organization := "amphip"

name := "amphip"

version := "0.0.1"


// SBT

incOptions := incOptions.value.withNameHashing(true)

updateOptions := updateOptions.value.withCachedResolution(true)

fork := true

initialCommands in console :=
  """import scalaz.syntax.show._
    |import amphip.dsl._
  """.stripMargin


// SCALA

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xlint", // Enable recommended additional warnings.
  "-Yno-adapted-args",
  "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ypatmat-exhaust-depth", "40") // scala 2.11.5

scalacOptions in (Compile, console) ~= (_.filterNot(op => Seq("-Xlint", "-Ywarn-unused-import").contains(op)))


// DEPENDENCIES

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "org.spire-math" %% "spire" % "0.9.1",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1"
)

 
 // TEST-DEPENDENCIES

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test)
