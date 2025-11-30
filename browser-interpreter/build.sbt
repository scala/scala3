import sbt.Keys._

val scala3Version = "3.7.0"

// Shared settings
lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := scala3Version,
  scalacOptions ++= Seq("-deprecation", "-feature")
)

// Shared code (cross-compiled to JVM and JS)
lazy val shared = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(commonSettings)
  .settings(
    name := "browser-interpreter-shared"
  )

lazy val sharedJVM = shared.jvm
lazy val sharedJS = shared.js

// JVM project for TASTy-to-JSON conversion tools
lazy val jvm = project
  .in(file("jvm"))
  .dependsOn(sharedJVM)
  .settings(commonSettings)
  .settings(
    name := "browser-interpreter-jvm",
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-tasty-inspector" % scala3Version
    ),
    Compile / mainClass := Some("browser.TastyToJsonConverter")
  )

// Scala.js project for browser execution
lazy val js = project
  .in(file("js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(sharedJS)
  .settings(commonSettings)
  .settings(
    name := "browser-interpreter-js",
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"
  )

// Root project
lazy val root = project
  .in(file("."))
  .aggregate(sharedJVM, sharedJS, jvm, js)
  .settings(commonSettings)
  .settings(
    name := "browser-interpreter",
    publish / skip := true
  )
