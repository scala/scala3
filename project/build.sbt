// Used by VersionUtil to get gitHash and commitDate
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.0.201803080745-r"


Compile / unmanagedSourceDirectories +=
  baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config"
libraryDependencies += Dependencies.`jackson-databind`
