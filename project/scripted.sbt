// Add EPFL Artifactory if on CI
resolvers ++= {
  if (sys.env.get("CI") == Some("drone")) Seq("EPFL Artifactory" at "http://scala-webapps.epfl.ch:8081")
  else Seq()
}

// Used by the subproject dotty-bridge
libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value
