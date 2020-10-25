// re-expose subproject settings
val `example-project` = ExampleProject.`example-project`

val dottyVersion = "3.0.0-M1-bin-SNAPSHOT"
val dokkaVersion = "1.4.10.2"
val flexmarkVersion = "0.42.12"
val jacksonVersion = "2.9.8"
val scalaTagsVersion = "0.9.1"
val dokkaSiteVersion = "0.1.9"

libraryDependencies ++= Seq(
  "org.jetbrains.dokka" % "dokka-test-api" % dokkaVersion % "test", // TODO move testing utils to dokka-site
  "com.virtuslab.dokka" % "dokka-site" % dokkaSiteVersion,

  "org.scala-lang" %% "scala3-tasty-inspector" % dottyVersion,
  "org.scala-lang" %% "scala3-compiler" % dottyVersion,
  "org.scala-sbt" % "io_2.13" % "1.3.4",

  "com.vladsch.flexmark" % "flexmark-all" % flexmarkVersion,
  "com.lihaoyi" % "scalatags_2.13" % scalaTagsVersion,
  "nl.big-o" % "liqp" % "0.6.7",
  "args4j" % "args4j" % "2.33",
  "com.novocode" % "junit-interface" % "0.11" % "test",
)

resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("kotlin", "kotlin-dev")
resolvers += Resolver.bintrayRepo("virtuslab", "dokka")

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3doc",
    version := "0.1.1-SNAPSHOT",
    scalaVersion := dottyVersion
  )

val generateSelfDocumentation = taskKey[Unit]("Generate example documentation")
generateSelfDocumentation := Def.taskDyn {
  val classroot = (Compile/target/classDirectory).value.getAbsolutePath.toString
  run.in(Compile).toTask(s" -o output/self -t $classroot -d documentation -n scala3doc -s src/main/scala=https://github.com/lampepfl/scala3doc/tree/master/src/main/scala#L") // TODO #35 proper sbt integration
}.value

// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
// javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true
// There is a bug in dokka that prevents parallel tests withing the same jvm
Test / parallelExecution := false
Test/fork := true
Test/envVars := Map(
  "scala3doc.classroot" -> (Compile/target/classDirectory).value.getAbsolutePath.toString,
)

scalacOptions in Compile += "-language:implicitConversions"

Compile / mainClass := Some("dotty.dokka.Main")

// hack, we cannot build documentation so we need this to publish locally
publishArtifact in (Compile, packageDoc) := false

// TODO #35 proper sbt integration
val generateDottyLibDocumentation = taskKey[Unit]("Generate documentation for dotty lib")
generateDottyLibDocumentation :=  Def.taskDyn {
  val dotttyLib = fullClasspath.in(Compile).value.find{ a =>
    val info = a.get(moduleID.key)
    info.nonEmpty &&
     info.get.organization == "ch.epfl.lamp" &&
     info.get.name.startsWith("dotty-library")
  }
  if (dotttyLib.isEmpty) Def.task {
    streams.value.log.error("Dotty lib wasn't found")
  } else Def.task {
    run.in(Compile).toTask(s" -o output/stdLib -t ${dotttyLib.get.data} -d dotty-docs/docs -n dotty-lib -s library/src=https://github.com/lampepfl/dotty/tree/master/library/src#L").value
  } 
}.value

