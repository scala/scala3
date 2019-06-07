lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val plugin = project
  .in(file("plugin"))
  .settings(
    name := "dividezero",
    version := "0.0.1",
    organization := "ch.epfl.lamp",
    scalaVersion := dottyVersion,

    scalacOptions ++= Seq(
      "-language:implicitConversions"
    ),

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-compiler" % scalaVersion.value % "provided"
    )
  )

lazy val app = project
  .in(file("app"))
  .settings(
    scalaVersion := dottyVersion
  )

lazy val appOk = project
  .in(file("appOk"))
  .settings(
    scalaVersion := dottyVersion
  )