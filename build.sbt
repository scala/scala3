name := "dotty"

organization := "lamp"

scalaVersion := "2.11.0-M7"

scalaSource in Compile := baseDirectory.value / "src"

scalacOptions in Global ++= Seq("-feature", "-deprecation", "-language:_")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.0-RC7"

scalaSource in Test := baseDirectory.value / "test"