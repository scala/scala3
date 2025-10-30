// Used by VersionUtil to get gitHash and commitDate
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.0.201803080745-r"

libraryDependencies += Dependencies.`jackson-databind`

// Used for manipulating YAML files in sidebar generation script
libraryDependencies += "org.yaml" % "snakeyaml" % "2.4"