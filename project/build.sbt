// Used by VersionUtil to get gitHash and commitDate
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.0.201803080745-r"
libraryDependencies += "org.ow2.asm" % "asm" % "9.9"

libraryDependencies += Dependencies.`jackson-databind`

// Used for manipulating YAML files in sidebar generation script
libraryDependencies += "org.yaml" % "snakeyaml" % "2.4"

Compile / unmanagedSourceDirectories ++= {
  val root = baseDirectory.value.getParentFile()
  Seq(
    root / "tasty/src",
    root / "tasty/src/dotty/tools/tasty/util",
  )
}
