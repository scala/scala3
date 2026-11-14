// Polymorphic function types and calls in TASTy produced by Scala 3.9 must still
// be readable now that they use the curried representation (scala/scala3#27202).
// `lib` is compiled with 3.9.0, `app` with the current compiler.

lazy val lib = project.in(file("lib"))
  .settings(scalaVersion := "3.9.0")

lazy val app = project.in(file("app"))
  .dependsOn(lib)
  .settings(scalacOptions += "-Ycheck:all")
