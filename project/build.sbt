// Used by VersionUtil to get gitHash and commitDate
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.0.201803080745-r"

// Include the source of DottyIDEPlugin in the project build,
// so that we can use the current in-development version of the plugin
// in our build instead of a released version.
// We do not add SbtDottyPlugin to show that it is not usefull anymore
// However, adding one source but not the others is very fragile

Compile / unmanagedSources += baseDirectory.value / "../sbt-dotty/src/dotty/tools/sbtplugin/DottyIDEPlugin.scala"

// Keep in sync with `sbt-dotty` config in Build.scala
libraryDependencies ++= Seq(
  Dependencies.`jackson-databind`
)
Compile / unmanagedSourceDirectories +=
  baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config"
