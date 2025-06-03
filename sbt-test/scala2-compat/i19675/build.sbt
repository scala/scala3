scalaVersion := sys.props("plugin.scalaVersion")

scalacOptions ++= Seq("-Wunused:imports", "-deprecation", "-Werror")
libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-thrift" % "24.2.0"
).map(_.cross(CrossVersion.for3Use2_13))
