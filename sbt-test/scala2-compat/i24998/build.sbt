// Local selective testing:
// scala-cli package --library -S 2.13 sbt-test/scala2-compat/i24998/lib -f -o test-lib.jar
// scala-cli package --library -S 2.13 sbt-test/scala2-compat/i24998/lib_inline -O -opt:l:inline -O '-opt-inline-from:scala/**' -f -o test-lib-inline.jar
// scala-cli run -S 3.8 sbt-test/scala2-compat/i24998/main --jar test-lib.jar test-lib-inline.jar
val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = sys.props("plugin.scala2Version")

lazy val libNoInline = (project in file ("lib"))
  .settings(scalaVersion := scala2Version)

lazy val libWithInlines = (project in file ("lib_inline"))
  .settings(scalaVersion := scala2Version)
  .settings(
    scalacOptions ++= Seq(
      // Yes, if somebody this option its code is non binary compatible https://docs.scala-lang.org/overviews/compiler-options/optimizer.html
      // We explicitly set it to reach these cases in our rewrites logic
      "-opt:inline:**",
      "-opt-inline-from:**",
    )
  )

lazy val test = (project in file ("main"))
  .dependsOn(libNoInline, libWithInlines)
  .settings(scalaVersion := scala3Version)
