// Add EPFL Artifactory if on CI
resolvers ++= {
  if (sys.env.get("CI") == Some("drone")) Seq("EPFL Artifactory" at "http://scala-webapps.epfl.ch:8081")
  else Seq()
}

// Include the sources of the sbt-dotty plugin in the project build,
// so that we can use the current in-development version of the plugin
// in our build instead of a released version.
unmanagedSourceDirectories in Compile += baseDirectory.value / "../sbt-dotty/src"

// Keep in sync with `sbt-dotty` config in Build.scala
libraryDependencies += Dependencies.`jackson-databind`
unmanagedSourceDirectories in Compile +=
  baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config"
