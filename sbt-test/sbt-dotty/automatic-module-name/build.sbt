import scala.sys.process._

// plugin.scalaVersion: set by scripted tests (local publish). test.scalaVersion: manual runs against Maven releases.
lazy val dottyVersion =
  sys.props.getOrElse("test.scalaVersion", sys.props("plugin.scalaVersion"))

val ExpectedAutomaticModuleName = "org.scala.lang.scala3.library"

val checkManifest = taskKey[Unit](
  "check that scala3-library_3 has Automatic-Module-Name in MANIFEST.MF"
)
val checkJpmsRuntime = taskKey[Unit](
  "check JPMS can resolve org.scala.lang.scala3.library from the published jar"
)

scalaVersion := dottyVersion

def findScala3LibraryJar(cp: Seq[Attributed[File]]): File =
  cp.map(_.data)
    .find(f => f.getName.startsWith("scala3-library_") && f.getName.endsWith(".jar"))
    .getOrElse(sys.error("scala3-library_3 jar not found on dependency classpath"))

checkManifest := {
  val jar = findScala3LibraryJar((Runtime / dependencyClasspath).value)
  val jarFile = new java.util.jar.JarFile(jar)
  try {
    val automaticModuleName =
      Option(jarFile.getManifest)
        .flatMap(m => Option(m.getMainAttributes.getValue("Automatic-Module-Name")))
    automaticModuleName match {
      case Some(`ExpectedAutomaticModuleName`) => ()
      case other =>
        sys.error(
          s"Expected Automatic-Module-Name: $ExpectedAutomaticModuleName in ${jar.getName}, got: $other"
        )
    }
  } finally jarFile.close()
}

checkJpmsRuntime := {
  val jar = findScala3LibraryJar((Runtime / dependencyClasspath).value)
  val exitCode = Process(
    Seq(
      "java",
      "--module-path",
      jar.getAbsolutePath,
      "--describe-module",
      ExpectedAutomaticModuleName
    )
  ).!
  if (exitCode != 0)
    sys.error(
      s"JPMS failed to describe module $ExpectedAutomaticModuleName from ${jar.getName} (exit $exitCode)"
    )
}
