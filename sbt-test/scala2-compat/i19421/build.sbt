scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence-tck" % "2.8.0"
).map(_.cross(CrossVersion.for3Use2_13))
