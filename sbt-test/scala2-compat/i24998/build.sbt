// Local selective testing:
// scala-cli package --library -S 2.13 sbt-test/scala2-compat/i24998/lib -O -opt:l:inline -O '-opt-inline-from:scala/**' -f -o test-lib.jar
// scala-cli run -S 3.8 sbt-test/scala2-compat/i24998/main --jar test-lib.jar
val scala3Version = sys.props("plugin.scalaVersion")
val scala2Version = sys.props("plugin.scala2Version")

lazy val lib = (project in file ("lib"))
  .settings(scalaVersion := scala2Version)
  .settings(
    scalacOptions ++= Seq(
      // Yes, if somebody this option its code is non binary compatible https://docs.scala-lang.org/overviews/compiler-options/optimizer.html
      // We explicitly set it to maximize chance of linking errors
      "-opt:l:inline",
      "-opt-inline-from:**",
    )
  )

lazy val test = (project in file ("main"))
  .dependsOn(lib)
  .settings(scalaVersion := scala3Version)
