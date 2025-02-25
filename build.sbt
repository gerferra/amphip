// based on https://github.com/tpolecat/skunk/blob/master/build.sbt

lazy val commonSettings = Seq(

  // Publishing
  organization := "amphip",
  version      := "1.0.1-SNAPSHOT",
  licenses     += "MPL-2.0" -> url("http://opensource.org/licenses/MPL-2.0"),
  homepage     := Some(url("https://github.com/gerferra/amphip")),
  developers   := List(
    Developer("gerferra", "Germán Ferrari", "", url("https://github.com/gerferra"))
  ),

  // Compilation
  scalaVersion       := "2.12.20",
  scalacOptions     ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    //"-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    //"-language:higherKinds",             // Allow higher-kinded types
    //"-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
    "-Ywarn-macros:before",              // via som
    "-Yrangepos"                         // for longer squiggles
    ,
    "-Xmaxerrs", "5"
  ),
  Compile / console / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports"),
  Compile / doc     / scalacOptions --= Seq("-Xfatal-warnings"),
  Test    / console / scalacOptions  := (Compile / console / scalacOptions).value,
  
  // Running
  fork := true,
  javaOptions ++= Seq(
      "-XX:MaxInlineLevel=18",

      // requires recent opejdk (>= 10?)
      //"-XX:+UnlockExperimentalVMOptions",
      //"-XX:+EnableJVMCI",
      //"-XX:+UseJVMCICompiler"
    )
)

lazy val amphip = project
  .in(file("."))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(core, bench, example, docs)
  .aggregate(core, bench, example, docs)

lazy val core = project
  .settings(commonSettings)
  .settings(
    name := "amphip-core",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      
      "org.typelevel"          %% "cats-core"    % "1.1.0",
      "org.typelevel"          %% "mouse"        % "0.17",
      "org.typelevel"          %% "spire"        % "0.15.0",
      "com.lihaoyi"            %% "sourcecode"   % "0.1.4",
      "com.github.pathikrit"   %% "better-files" % "3.9.1",
      "com.chuusai"            %% "shapeless"    % "2.3.3", 
    ),
    libraryDependencies ++= Seq(
      "com.novocode"   % "junit-interface" % "0.11"  % Test
    ),
    console / initialCommands := 
      """
      |import cats.instances.all._
      |import cats.syntax.show._
      |import amphip.dsl._
      """.stripMargin,
    // BigModelSpec
    Test / run / connectInput := true
  )

lazy val bench = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(
    name := "amphip-bench",
    publish / skip := true,
    run / fork := true,
    javaOptions ++= Seq(
      "-Xmx4G"
    )
  )

lazy val example = project
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "amphip-example",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "1.3"
    )
  )

lazy val docs = project
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "amphip-docs",
    publish / skip := true
  )
