import sbt._
import Keys._

object B extends Build
{
	lazy val dep = Project("dep", file("dep"))
	lazy val use = Project("use", file("use")) settings(
		unmanagedJars in Compile <+= packageBin in (dep, Compile) map Attributed.blank,

    // Disable classpath caching since it does not invalidate modified jars
    // (https://github.com/scala/bug/issues/10295)
    scalacOptions += "-YdisableFlatCpCaching"
	)
}
