// Used by VersionUtil to get gitHash and commitDate
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "7.3.0.202506031305-r"
libraryDependencies += "org.ow2.asm" % "asm" % "9.9"

// Used for manipulating YAML files in sidebar generation script
libraryDependencies += "org.yaml" % "snakeyaml" % "2.4"

// sbt 1.x -> 2.x API shims for meta-build plugins (jar from the sbt2-compat plugin artifact)
libraryDependencies += "com.github.sbt" % "sbt2-compat_sbt2_3" % "0.1.0"
