lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val pluginSetting = Seq(
  name := "dividezero",
  version := "0.0.1",
  organization := "ch.epfl.lamp",
  scalaVersion := dottyVersion,

  libraryDependencies ++= Seq(
    "ch.epfl.lamp" %% "dotty-compiler" % scalaVersion.value % "provided"
  )
)

lazy val plugin = (project in file("plugin")).settings(pluginSetting: _*)

scalaVersion := dottyVersion

autoCompilerPlugins := true

addCompilerPlugin("ch.epfl.lamp" %% "dividezero" % "0.0.1")
