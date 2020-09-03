import sbt._
import sbt.Keys._

object ExampleProject {
  import Protoplugin.Keys._

  val `example-project` =
    project.in(file("example-project"))
    .settings(Protoplugin.Settings ++ Seq(
      name := "scala3doc-example-project",
      description := "Example SBT project that is documented using Scala3doc",
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "0.27.0-RC1",

      scala3docOptions ++= Seq("--name", "example-project"),
      Compile / doc / target := file("output/example-project"),
    ): _*)
}
