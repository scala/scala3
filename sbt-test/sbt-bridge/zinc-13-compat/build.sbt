scalaVersion := "2.12.13"
val Scala3Compiler = config("scala3-compiler")
val Scala3Bridge = config("scala3-bridge")
val scala3Version = sys.props("plugin.scalaVersion")
lazy val root = project.in(file("."))
  .configs(Scala3Compiler, Scala3Bridge)
  .settings(
    inConfig(Scala3Compiler)(Defaults.configSettings),
    inConfig(Scala3Bridge)(Defaults.configSettings),
    libraryDependencies ++= Seq(
      "org.scala-sbt" %% "zinc" % "1.3.5",
      "org.scala-lang" % "scala3-compiler_3" % scala3Version % Scala3Compiler,
      "org.scala-lang" % "scala3-sbt-bridge" % scala3Version % Scala3Bridge
    ),
    autoScalaLibrary := false,
    run / fork := true,
    Compile / sourceGenerators += Def.task {
      val converter = fileConverter.value
      def classpathFile(entry: Attributed[xsbti.HashedVirtualFileRef]): File =
        converter.toPath(entry.data).toFile
      val scala3File = (Compile / sourceManaged).value / "Scala3.scala"
      val inputFile = (Compile / sourceManaged).value / "Input.scala"
      val allJars = (Scala3Compiler / managedClasspath).value.map(classpathFile)
      val compilerJar = allJars
        .find(jar => jar.getName.contains("scala3-compiler"))
        .get.getAbsolutePath.toString
      val libraryJars = allJars
        .filter(jar => jar.getName.contains("library"))
        .map(_.getAbsolutePath.toString)
      val bridgeJars = (Scala3Bridge / managedClasspath).value.map(classpathFile)
      val bridgeJar = bridgeJars
        .find(att => att.getName.contains("scala3-sbt-bridge"))
        .get.getAbsolutePath.toString
      IO.write(
        scala3File,
        s"""|
            |import java.io.File
            |
            |object Scala3 {
            |  val version = "$scala3Version"
            |  val allJars = Array(
            |    ${allJars.map(jar => s"""new File("${jar.getAbsolutePath}")""").mkString(",\n    ")}
            |  )
            |  val compilerJar = new File("$compilerJar")
            |  val libraryJars = Array(
            |    ${libraryJars.map(jar => s"""new File("$jar")""").mkString(",\n    ")}
            |  )
            |  val bridgeJar = new File("$bridgeJar")
            |}
            |""".stripMargin
      )
      val output = target.value / "test-output"
      IO.createDirectory(output)
      val sourceFile = baseDirectory.value / "tests" / "Hello.scala"
      IO.write(
        inputFile,
        s"""|
            |import java.io.File
            |
            |object Input {
            |  val outputFile = new File("${output.getAbsolutePath}")
            |  val sources = Array(new File("${sourceFile.getAbsolutePath}"))
            |}
            |""".stripMargin
      )
      Seq(scala3File, inputFile)
    }
  )
