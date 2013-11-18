name := "dotty"

organization := "lamp"

scalaVersion := "2.11.0-M6"

scalaBinaryVersion := scalaVersion.value

scalaSource in Compile <<= baseDirectory / "src"

scalacOptions in Global ++= Seq("-feature", "-deprecation", "-language:_")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-xml" % "1.0.0-RC6"
)

scalaSource in Test <<= baseDirectory / "test"

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

