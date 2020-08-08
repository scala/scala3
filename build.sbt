val dottyVersion = "0.26.0-RC1"
val dokkaVersion = "1.4.0-M3-dev-81"
val flexmarkVersion = "0.42.12"
val jacksonVersion = "2.9.8"

libraryDependencies += "org.jetbrains.dokka" % "dokka-base" % dokkaVersion
libraryDependencies += "org.jetbrains.dokka" % "dokka-core" % dokkaVersion
libraryDependencies += "org.jetbrains.dokka" % "dokka-test-api" % dokkaVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-tastydoc" % dottyVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-library" % dottyVersion
libraryDependencies += "org.scala-sbt" % "io_2.13" % "1.3.4"

resolvers += Resolver.jcenterRepo

resolvers += Resolver.bintrayRepo("kotlin", "kotlin-dev")

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-dokka",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )


 val dokkaJavaApiJar = file("libs") / "dokkaJavaApi-0.1.0.jar"


val buildDokkaApi = taskKey[File]("Compile dokka wrapper and put jar in lib")
buildDokkaApi := {
  val gradleRootDir = file("dokkaJavaApi")
  sys.process.Process(Seq("./gradlew", "build"), gradleRootDir).!

  if (dokkaJavaApiJar.exists()) IO.delete(dokkaJavaApiJar)
  IO.move(gradleRootDir / "build" / "libs" / "dokkaJavaApi-0.1.0.jar", dokkaJavaApiJar)
  streams.value.log.success(s"Dokka api copied to $dokkaJavaApiJar")
  dokkaJavaApiJar
}

val generateSelfDocumentation = inputKey[Unit]("Generate example documentation")
generateSelfDocumentation := {
  run.in(Compile).fullInput(" -o output/self -t target/scala-0.26/classes -d documentation").evaluated // TODO #35 proper sbt integration
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
    run.in(Compile).toTask(s" -o output/stdLib -t ${dotttyLib.get.data} -d dotty-docs/docs").value
  } 
}.value

libraryDependencies ++= {
      Seq(
        "com.vladsch.flexmark" % "flexmark-all" % flexmarkVersion,
        "nl.big-o" % "liqp" % "0.6.7",
        "org.jetbrains.kotlinx" % "kotlinx-html-jvm" % "0.6.10"
      )
    }

libraryDependencies += "args4j" % "args4j" % "2.33"
