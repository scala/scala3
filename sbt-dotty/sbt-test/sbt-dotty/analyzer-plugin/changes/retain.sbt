lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val plugin = project
  .in(file("plugin"))
  .settings(
    name := "init-checker",
    version := "0.0.1",
    organization := "ch.epfl.lamp",
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-compiler" % scalaVersion.value % "provided"
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
    scalaVersion := dottyVersion,
    scalacOptions += "-Yretain-trees",
    addCompilerPlugin("ch.epfl.lamp" %% "init-checker" % "0.0.1")
  )
  .dependsOn(lib)
