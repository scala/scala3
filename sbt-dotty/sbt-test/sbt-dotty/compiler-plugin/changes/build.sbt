lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val plugin = project
  .in(file("plugin"))
  .settings(
    name := "dividezero",
    version := "0.0.1",
    organization := "ch.epfl.lamp",
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty" % scalaVersion.value % "provided"
    )
  )

lazy val app = project
  .in(file("app"))
  .settings(
    scalaVersion := dottyVersion,
    addCompilerPlugin("ch.epfl.lamp" %% "dividezero" % "0.0.1")
  )

lazy val appOk = project
  .in(file("appOk"))
  .settings(
    scalaVersion := dottyVersion,
    addCompilerPlugin("ch.epfl.lamp" %% "dividezero" % "0.0.1")
  )