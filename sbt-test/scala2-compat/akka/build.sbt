scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies ++= {
  Seq(
    ("com.typesafe.akka" %% "akka-http"   % "10.1.10"),
    ("com.typesafe.akka" %% "akka-stream" % "2.6.0")
  ).map(_.cross(CrossVersion.for3Use2_13))
}
