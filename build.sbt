val dottyVersion = "0.25.0-RC2"

libraryDependencies += "org.jetbrains.dokka" % "dokka-base" % "1.4.0-M3-dev-81"
libraryDependencies += "org.jetbrains.dokka" % "dokka-core" % "1.4.0-M3-dev-81"

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

val buildDokkaApi = taskKey[Unit]("Compile dokka wrapper and put jar in lib")
buildDokkaApi := {
  val gradleRootDir = file("dokkaJavaApi")
  sys.process.Process(Seq("./gradlew", "clean", "build"), gradleRootDir).!

  IO.delete(dokkaJavaApiJar)
  IO.move(gradleRootDir / "build" / "libs" / "dokkaJavaApi-0.1.0.jar", dokkaJavaApiJar)
  streams.value.log.success(s"Dokka api copied to $dokkaJavaApiJar")
}

unmanagedJars in Compile += dokkaJavaApiJar

javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=krzysiek-MS-7971:5005,suspend=y"
fork.in(run) := true