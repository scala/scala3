lazy val dottyVersion = sys.props("plugin.scalaVersion")

lazy val lib = project
  .in(file("lib"))
  .settings(
    scalaVersion := dottyVersion
  )

val jarDest = file("target") / "app.jar"

val runTest = Def.taskKey[Unit]("run tests")

lazy val inspector = project
  .in(file("inspector"))
  .settings(
    scalaVersion := dottyVersion,
    libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
    runTest := Def.uncached {
      Def.sequential(
        Def.task {
          val converter = fileConverter.value
          val bin = (lib / Compile / packageBin).value
          IO.copyFile(converter.toPath(bin).toFile, jarDest)
        },
        (Compile / run).toTask(" " + jarDest.getAbsolutePath)
      ).value
    }
  )
  .dependsOn(lib)
