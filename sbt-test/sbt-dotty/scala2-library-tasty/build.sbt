scalaVersion := sys.props("plugin.scalaVersion")

libraryDependencies += "org.scala-lang" %% "scala2-library-tasty-experimental" % scalaVersion.value
scalacOptions += "-Yscala2-unpickler:never" // check that we do not load symbol from the Scala 2 library classfiles (use TASTy)
