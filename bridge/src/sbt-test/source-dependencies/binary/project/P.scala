import sbt._
import Keys._

object B extends Build
{
	lazy val dep = Project("dep", file("dep"))
	lazy val use = Project("use", file("use")) settings(
		unmanagedJars in Compile <+= packageBin in (dep, Compile) map Attributed.blank
	)
}