val dottyVersion = "0.25.0-RC2"

libraryDependencies += "org.jetbrains.dokka" % "dokka-base" % "1.4.0-M3-dev-81"
libraryDependencies += "org.jetbrains.dokka" % "dokka-core" % "1.4.0-M3-dev-81"
libraryDependencies += "ch.epfl.lamp" %% "dotty-tastydoc" % dottyVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion
libraryDependencies += "ch.epfl.lamp" %% "dotty-library" % dottyVersion

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

val generateExapleDocumentation = taskKey[Unit]("Generate example documentation")
generateExapleDocumentation := run.in(Compile).toTask("").value // TODO 

unmanagedJars in Compile += dokkaJavaApiJar

// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
//javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true

scalacOptions in Compile += "-language:implicitConversion"