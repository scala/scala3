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
