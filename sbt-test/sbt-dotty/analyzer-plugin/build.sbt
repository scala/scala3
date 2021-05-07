lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val plugin = project
  .in(file("plugin"))
  .settings(
    name := "init-checker",
    version := "0.0.1",
    organization := "org.scala-lang",
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided"
    )
  )

lazy val lib = project
  .in(file("lib"))
  .settings(
    scalaVersion := dottyVersion
  )

lazy val app = project
  .in(file("app"))
  .settings(
    scalaVersion := dottyVersion
  )
  .dependsOn(lib)
