name := "dotty"

organization := "lamp"

scalaVersion := "2.10.0"

scalaSource in Compile <<= baseDirectory / "src"

scalacOptions in Global ++= Seq("-feature", "-deprecation", "-language:_")

libraryDependencies <+= scalaVersion ( sv => "org.scala-lang" % "scala-reflect" % sv )

scalaSource in Test <<= baseDirectory / "test"