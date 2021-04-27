lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val plugin = project
  .in(file("plugin"))
  .settings(
    name := "dividezero",
    version := "0.0.1",
    organization := "org.scala-lang",
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "dotty" % scalaVersion.value % "provided"
    )
  )

lazy val app = project
  .in(file("app"))
  .settings(
    scalaVersion := dottyVersion,
    addCompilerPlugin("org.scala-lang" %% "dividezero" % "0.0.1")
  )

lazy val appOk = project
  .in(file("appOk"))
  .settings(
    scalaVersion := dottyVersion,
    addCompilerPlugin("org.scala-lang" %% "dividezero" % "0.0.1")
  )