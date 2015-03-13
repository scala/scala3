
name := "dotty"

scalaVersion in Global := "2.11.5"

version in Global := "0.1-SNAPSHOT"

organization in Global := "org.scala-lang"

organizationName in Global := "LAMP/EPFL"

organizationHomepage in Global := Some(url("http://lamp.epfl.ch"))

homepage in Global := Some(url("http://scala-lang.org"))

// ----- Partest -------
libraryDependencies += "org.scala-lang.modules" %% "scala-partest" % "1.0.5" % "test"

fork in Test := true

javaOptions in Test += "-Xmx1G"

testFrameworks += new TestFramework("dotty.partest.Framework")

definedTests in Test += (new sbt.TestDefinition("partest",
  // marker fingerprint since there are no test classes to be discovered by sbt:
  new sbt.testing.AnnotatedFingerprint {
    def isModule = true
    def annotationName = "partest"
  }, true, Array())
)
