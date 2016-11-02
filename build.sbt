// GENERAL

organization := "amphip"

name := "amphip"

version := "0.0.2"


// SBT

incOptions := incOptions.value.withNameHashing(true)

updateOptions := updateOptions.value.withCachedResolution(true)

fork := true

initialCommands in console :=
  """import scalaz.syntax.show._
    |import amphip.dsl._
  """.stripMargin


// SCALA

scalaOrganization := "org.typelevel"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xlint", 
  "-Yno-adapted-args",
  "-Ywarn-dead-code", 
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ypatmat-exhaust-depth", "40",  // scala 2.11.5
  "-Ypartial-unification" // enable fix for SI-2712
) 

scalacOptions in (Compile, console) ~= (_.filterNot(op => Seq("-Xlint", "-Ywarn-unused-import").contains(op)))


// DEPENDENCIES

libraryDependencies ++= Seq(
  "org.spire-math"                %% "spire"         % "0.12.0",
  "com.lihaoyi"                   %% "sourcecode"    % "0.1.3",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1",
  // transitioning ...
  "org.scalaz"                    %% "scalaz-core"   % "7.2.6",
  "org.typelevel"                 %% "cats"          % "0.7.2"
)

 
 // TEST-DEPENDENCIES

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"       % "3.0.0" % Test,
  "com.novocode"   % "junit-interface" % "0.11"  % Test)
