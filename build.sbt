
name := "dotty"

scalaVersion in Global := "2.11.0-M7"

version in Global := "0.1-SNAPSHOT"

organization in Global := "org.scala-lang"

organizationName in Global := "LAMP/EPFL"

organizationHomepage in Global := Some(url("http://lamp.epfl.ch"))

homepage in Global := Some(url("http://scala-lang.org"))

mainClass in (Compile, packageBin) := Some("dotty.tools.dotc.Main")

packageOptions in (Compile, packageBin) <+= (target, externalDependencyClasspath in Runtime) map { 
 (targetDirectory: File, classpath: Classpath) =>
    val absolutePaths = classpath map { attrFile: Attributed[File] => attrFile.data.toPath().toString() }; 
    Package.ManifestAttributes(java.util.jar.Attributes.Name.CLASS_PATH -> absolutePaths.reduceOption(_ + " " + _).getOrElse(""))
 }
