val dottyVersion = "0.27.0-RC1"
val dokkaVersion = "1.4.0"
val kotlinxVersion = "0.7.2" // upgrade when upgrading dokka
val flexmarkVersion = "0.42.12"
val jacksonVersion = "2.9.8"

libraryDependencies ++= Seq(
  "org.jetbrains.dokka" % "dokka-base" % dokkaVersion,
  "org.jetbrains.dokka" % "dokka-core" % dokkaVersion,
  "org.jetbrains.dokka" % "dokka-test-api" % dokkaVersion,
  "org.jetbrains.kotlinx" % "kotlinx-html-jvm" % kotlinxVersion,
  "com.virtuslab.dokka" % "dokka-site" % "0.1.6",

  "ch.epfl.lamp" %% "dotty-tasty-inspector" % dottyVersion,
  "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion,
  "ch.epfl.lamp" %% "dotty-library" % dottyVersion,
  "org.scala-sbt" % "io_2.13" % "1.3.4",

  "com.vladsch.flexmark" % "flexmark-all" % flexmarkVersion,
  "nl.big-o" % "liqp" % "0.6.7",
  "args4j" % "args4j" % "2.33",
  "com.novocode" % "junit-interface" % "0.11" % "test"
)

resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("kotlin", "kotlin-dev")
resolvers += Resolver.bintrayRepo("virtuslab", "dokka")
resolvers += Resolver.mavenLocal

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3doc",
    version := "0.1.1",
    scalaVersion := dottyVersion
  )


val dokkaJavaApiJar = file("libs") / "dokkaJavaApi-0.1.1.jar"
val gradleRootDir = file("dokkaJavaApi")

val buildDokkaApi = taskKey[File]("Compile dokka wrapper and put jar in lib")
buildDokkaApi := {
  streams.value.log.info("Building Dokka API with Gradle...")
  sys.process.Process(Seq("./gradlew", "build"), gradleRootDir).!

  if (dokkaJavaApiJar.exists()) IO.delete(dokkaJavaApiJar)
  IO.move(gradleRootDir / "build" / "libs" / "dokkaJavaApi-0.1.1.jar", dokkaJavaApiJar)
  streams.value.log.success(s"Dokka API copied to $dokkaJavaApiJar")
  dokkaJavaApiJar
}

compile.in(Compile) := (compile.in(Compile).dependsOn(buildDokkaApi)).value

val generateSelfDocumentation = inputKey[Unit]("Generate example documentation")
generateSelfDocumentation := {
  run.in(Compile).fullInput(" -o output/self -t target/scala-0.26/classes -d documentation -n scala3doc -s src/main/scala=https://github.com/lampepfl/scala3doc/tree/master/src/main/scala#L").evaluated // TODO #35 proper sbt integration
}

unmanagedJars in Compile += dokkaJavaApiJar

// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
//javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true
// There is a bug in dokka that prevents parallel tests withing the same jvm
fork.in(test) := true
Test / parallelExecution := false

scalacOptions in Compile += "-language:implicitConversions"


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

