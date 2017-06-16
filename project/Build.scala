import sbt.Keys._
import sbt._
import complete.DefaultParsers._
import java.io.{File, RandomAccessFile}
import java.nio.channels.FileLock
import java.nio.file.{ Files, FileSystemException }
import java.util.Calendar

import scala.reflect.io.Path
import sbtassembly.AssemblyKeys.assembly
import xerial.sbt.Pack._

import sbt.Package.ManifestAttributes

import com.typesafe.sbteclipse.plugin.EclipsePlugin._

import dotty.tools.sbtplugin.DottyPlugin.autoImport._
import dotty.tools.sbtplugin.DottyIDEPlugin.{ prepareCommand, runProcess }
import dotty.tools.sbtplugin.DottyIDEPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

/* In sbt 0.13 the Build trait would expose all vals to the shell, where you
 * can use them in "set a := b" like expressions. This re-exposes them.
 */
object ExposedValues extends AutoPlugin {
  object autoImport {
    val bootstrapFromPublishedJars = Build.bootstrapFromPublishedJars
    val bootstrapOptimised = Build.bootstrapOptimised
  }
}

object Build {

  val scalacVersion = "2.11.11" // Do not rename, this is grepped in bin/common.

  val dottyOrganization = "ch.epfl.lamp"
  val dottyGithubUrl = "https://github.com/lampepfl/dotty"
  val dottyVersion = {
    val baseVersion = "0.2.0"
    val isNightly = sys.env.get("NIGHTLYBUILD") == Some("yes")
    val isRelease = sys.env.get("RELEASEBUILD") == Some("yes")
    if (isNightly)
      baseVersion + "-bin-" + VersionUtil.commitDate + "-" + VersionUtil.gitHash + "-NIGHTLY"
    else if (isRelease)
      baseVersion
    else
      baseVersion + "-bin-SNAPSHOT"
  }
  val dottyNonBootstrappedVersion = dottyVersion + "-nonbootstrapped"

  val jenkinsMemLimit = List("-Xmx1500m")

  val JENKINS_BUILD = "dotty.jenkins.build"
  val DRONE_MEM = "dotty.drone.mem"


  val agentOptions = List(
    // "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
    // "-agentpath:/home/dark/opt/yjp-2013-build-13072/bin/linux-x86-64/libyjpagent.so"
    // "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15052.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
    // "-XX:+HeapDumpOnOutOfMemoryError", "-Xmx1g", "-Xss2m"
  )

  // Packages all subprojects to their jars
  lazy val packageAll =
    taskKey[Map[String, String]]("Package everything needed to run tests")

  // Spawns a repl with the correct classpath
  lazy val repl = inputKey[Unit]("run the REPL with correct classpath")

  // Run tests with filter through vulpix test suite
  lazy val vulpix = inputKey[Unit]("runs integration test with the supplied filter")

  // Used to compile files similar to ./bin/dotc script
  lazy val dotc =
    inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

  // Used to run binaries similar to ./bin/dotr script
  lazy val dotr =
    inputKey[Unit]("run compiled binary using the correct classpath, or the user supplied classpath")

  // Compiles the documentation and static site
  lazy val genDocs = inputKey[Unit]("run dottydoc to generate static documentation site")

  // Shorthand for compiling a docs site
  lazy val dottydoc = inputKey[Unit]("run dottydoc")

  lazy val bootstrapFromPublishedJars = settingKey[Boolean]("If true, bootstrap dotty from published non-bootstrapped dotty")
  lazy val bootstrapOptimised = settingKey[Boolean]("Bootstrap with -optimise")

  // Used in build.sbt
  lazy val thisBuildSettings = Def.settings(
    // Change this to true if you want to bootstrap using a published non-bootstrapped compiler
    bootstrapFromPublishedJars := false,

    bootstrapOptimised := false,

    // Override `runCode` from sbt-dotty to use the language-server and
    // vscode extension from the source repository of dotty instead of a
    // published version.
    runCode := (run in `dotty-language-server`).toTask("").value
  )

  // Only available in vscode-dotty
  lazy val unpublish = taskKey[Unit]("Unpublish a package")



  lazy val commonSettings = publishSettings ++ Seq(
    organization := dottyOrganization,
    organizationName := "LAMP/EPFL",
    organizationHomepage := Some(url("http://lamp.epfl.ch")),
    homepage := Some(url(dottyGithubUrl)),

    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-encoding", "UTF8",
      "-language:existentials,higherKinds,implicitConversions"
    ),

    javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    scalaSource       in Compile    := baseDirectory.value / "src",
    scalaSource       in Test       := baseDirectory.value / "test",
    javaSource        in Compile    := baseDirectory.value / "src",
    javaSource        in Test       := baseDirectory.value / "test",
    resourceDirectory in Compile    := baseDirectory.value / "resources",

    // Prevent sbt from rewriting our dependencies
    ivyScala ~= (_ map (_ copy (overrideScalaVersion = false)))
  )

  // Settings used for projects compiled only with Scala 2
  lazy val commonScala2Settings = commonSettings ++ Seq(
    version := dottyVersion,
    scalaVersion := scalacVersion
  )

  // Settings used when compiling dotty using Scala 2
  lazy val commonNonBootstrappedSettings = commonSettings ++ publishSettings ++ Seq(
    version := dottyNonBootstrappedVersion,
    scalaVersion := scalacVersion
  )

  // Settings used when compiling dotty with a non-bootstrapped dotty
  lazy val commonBootstrappedSettings = commonSettings ++ Seq(
    EclipseKeys.skipProject := true,
    version := dottyVersion,
    scalaVersion := dottyNonBootstrappedVersion,

    // Avoid having to run `dotty-sbt-bridge/publishLocal` before compiling a bootstrapped project
    scalaCompilerBridgeSource :=
      (dottyOrganization %% "dotty-sbt-bridge" % "NOT_PUBLISHED" % Configurations.Component.name)
      .artifacts(Artifact.sources("dotty-sbt-bridge").copy(url =
        // We cannot use the `packageSrc` task because a setting cannot depend
        // on a task. Instead, we make `compile` below depend on the bridge `packageSrc`
        Some((artifactPath in (`dotty-sbt-bridge`, Compile, packageSrc)).value.toURI.toURL))),
    compile in Compile := (compile in Compile)
      .dependsOn(packageSrc in (`dotty-sbt-bridge`, Compile)).value,

    // Use the same name as the non-bootstrapped projects for the artifacts
    moduleName ~= { _.stripSuffix("-bootstrapped") },

    // Prevent sbt from setting the Scala bootclasspath, otherwise it will
    // contain `scalaInstance.value.libraryJar` which in our case is the
    // non-bootstrapped dotty-library that will then take priority over
    // the bootstrapped dotty-library on the classpath or sourcepath.
    classpathOptions ~= (_.copy(autoBoot = false)),
    // We still need a Scala bootclasspath equal to the JVM bootclasspath,
    // otherwise sbt 0.13 incremental compilation breaks (https://github.com/sbt/sbt/issues/3142)
    scalacOptions ++= Seq("-bootclasspath", sys.props("sun.boot.class.path")),

    // sbt gets very unhappy if two projects use the same target
    target := baseDirectory.value / ".." / "out" / "bootstrap" / name.value,

    // The non-bootstrapped dotty-library is not necessary when bootstrapping dotty
    autoScalaLibrary := false,
    // ...but scala-library is
    libraryDependencies += "org.scala-lang" % "scala-library" % scalacVersion,

    scalacOptions ++= {
      if (bootstrapOptimised.value)
        Seq("-optimise")
      else
        Seq()
    },

    ivyConfigurations ++= {
      if (bootstrapFromPublishedJars.value)
        Seq(Configurations.ScalaTool)
      else
        Seq()
    },
    libraryDependencies ++= {
      if (bootstrapFromPublishedJars.value)
        Seq(
          dottyOrganization %% "dotty-library" % dottyNonBootstrappedVersion % Configurations.ScalaTool.name,
          dottyOrganization %% "dotty-compiler" % dottyNonBootstrappedVersion % Configurations.ScalaTool.name
        ).map(_.withDottyCompat())
      else
        Seq()
    },

    // Compile using the non-bootstrapped and non-published dotty
    managedScalaInstance := false,
    scalaInstance := {
      val (libraryJar, compilerJar) =
        if (bootstrapFromPublishedJars.value) {
          val jars = update.value.select(
            configuration = configurationFilter(Configurations.ScalaTool.name),
            artifact = artifactFilter(extension = "jar")
          )
          (jars.find(_.getName.startsWith("dotty-library_2.11")).get,
           jars.find(_.getName.startsWith("dotty-compiler_2.11")).get)
        } else
          ((packageBin in (`dotty-library`, Compile)).value,
           (packageBin in (`dotty-compiler`, Compile)).value)

      // All compiler dependencies except the library
      val otherDependencies = (dependencyClasspath in (`dotty-compiler`, Compile)).value
        .filterNot(_.get(artifact.key).exists(_.name == "dotty-library"))
        .map(_.data)

      // This ScalaInstance#apply overload is deprecated in sbt 0.13, but the non-deprecated
      // constructor in sbt 1.0 does not exist in sbt 0.13
      ScalaInstance(scalaVersion.value, libraryJar, compilerJar, otherDependencies: _*)(state.value.classLoaderCache.apply)
    }
  )

  // sbt >= 0.13.12 will automatically rewrite transitive dependencies on
  // any version in any organization of scala{-library,-compiler,-reflect,p}
  // to have organization `scalaOrganization` and version `scalaVersion`
  // (see https://github.com/sbt/sbt/pull/2634).
  // This means that we need to provide dummy artefacts for these projects,
  // otherwise users will get compilation errors if they happen to transitively
  // depend on one of these projects.
  lazy val commonDummySettings = commonBootstrappedSettings ++ Seq(
    crossPaths := false,
    libraryDependencies := Seq()
  )

  /** Projects -------------------------------------------------------------- */

  // Needed because the dotty project aggregates dotty-sbt-bridge but dotty-sbt-bridge
  // currently refers to dotty in its scripted task and "aggregate" does not take by-name
  // parameters: https://github.com/sbt/sbt/issues/2200
  lazy val dottySbtBridgeRef = LocalProject("dotty-sbt-bridge")
  // Same thing for the bootstrapped version
  lazy val dottySbtBridgeBootstrappedRef = LocalProject("dotty-sbt-bridge-bootstrapped")

  // The root project:
  // - aggregates other projects so that "compile", "test", etc are run on all projects at once.
  // - publishes its own empty artifact "dotty" that depends on "dotty-library" and "dotty-compiler",
  //   this is only necessary for compatibility with sbt which currently hardcodes the "dotty" artifact name
  lazy val dotty = project.in(file(".")).
    // FIXME: we do not aggregate `bin` because its tests delete jars, thus breaking other tests
    aggregate(`dotty-interfaces`, `dotty-library`, `dotty-compiler`, `dotty-doc`, dottySbtBridgeRef).
    dependsOn(`dotty-compiler`).
    dependsOn(`dotty-library`).
    settings(commonNonBootstrappedSettings).
    settings(
      triggeredMessage in ThisBuild := Watched.clearWhenTriggered,
      submoduleChecks,

      addCommandAlias("run", "dotty-compiler/run") ++
      addCommandAlias("legacyTests", "dotty-compiler/testOnly dotc.tests")
    )

  // Same as `dotty` but using bootstrapped projects.
  lazy val `dotty-bootstrapped` = project.
    aggregate(`dotty-interfaces`, `dotty-library-bootstrapped`, `dotty-compiler-bootstrapped`, `dotty-doc-bootstrapped`,
      `dotty-language-server`,
      dottySbtBridgeBootstrappedRef,
      `scala-library`, `scala-compiler`, `scala-reflect`, scalap).
    dependsOn(`dotty-compiler-bootstrapped`).
    dependsOn(`dotty-library-bootstrapped`).
    settings(commonBootstrappedSettings)

  lazy val `dotty-interfaces` = project.in(file("interfaces")).
    settings(commonScala2Settings). // Java-only project, so this is fine
    settings(
      // Do not append Scala versions to the generated artifacts
      crossPaths := false,
      // Do not depend on the Scala library
      autoScalaLibrary := false,
      // Let the sbt eclipse plugin know that this is a Java-only project
      EclipseKeys.projectFlavor := EclipseProjectFlavor.Java,
      //Remove javac invalid options in Compile doc
      javacOptions in (Compile, doc) --= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    )

  // Settings shared between dotty-doc and dotty-doc-bootstrapped
  lazy val dottyDocSettings = Seq(
    baseDirectory in (Compile, run) := baseDirectory.value / "..",
    baseDirectory in (Test, run) := baseDirectory.value,

    connectInput in run := true,
    outputStrategy := Some(StdoutOutput),

    javaOptions ++= (javaOptions in `dotty-compiler`).value,
    fork in run := true,
    fork in Test := true,
    parallelExecution in Test := false,

    genDocs := Def.inputTaskDyn {
      val dottyLib = (packageAll in `dotty-compiler`).value("dotty-library")
      val dottyInterfaces = (packageAll in `dotty-compiler`).value("dotty-interfaces")
      val otherDeps = (dependencyClasspath in Compile).value.map(_.data).mkString(":")
      val sources =
        (unmanagedSources in (Compile, compile)).value ++
          (unmanagedSources in (`dotty-compiler`, Compile)).value
      val args: Seq[String] = Seq(
        "-siteroot", "docs",
        "-project", "Dotty",
        "-project-version", dottyVersion,
        "-project-url", dottyGithubUrl,
        "-classpath", s"$dottyLib:$dottyInterfaces:$otherDeps"
      )
        (runMain in Compile).toTask(
          s""" dotty.tools.dottydoc.Main ${args.mkString(" ")} ${sources.mkString(" ")}"""
        )
    }.evaluated,

    dottydoc := Def.inputTaskDyn {
      val args: Seq[String] = spaceDelimited("<arg>").parsed
      val dottyLib = (packageAll in `dotty-compiler`).value("dotty-library")
      val dottyInterfaces = (packageAll in `dotty-compiler`).value("dotty-interfaces")
      val otherDeps = (dependencyClasspath in Compile).value.map(_.data).mkString(":")
      val cp: Seq[String] = Seq("-classpath", s"$dottyLib:$dottyInterfaces:$otherDeps")
        (runMain in Compile).toTask(s""" dotty.tools.dottydoc.Main ${cp.mkString(" ")} """ + args.mkString(" "))
    }.evaluated,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "com.vladsch.flexmark" % "flexmark" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-gfm-tasklist" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-gfm-tables" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-autolink" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-anchorlink" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-emoji" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-gfm-strikethrough" % "0.11.1",
      "com.vladsch.flexmark" % "flexmark-ext-yaml-front-matter" % "0.11.1",
      Dependencies.`jackson-dataformat-yaml`,
      "nl.big-o" % "liqp" % "0.6.7"
    )
  )

  lazy val `dotty-doc` = project.in(file("doc-tool")).
    dependsOn(`dotty-compiler`, `dotty-compiler` % "test->test").
    settings(commonNonBootstrappedSettings).
    settings(dottyDocSettings)

  lazy val `dotty-doc-bootstrapped` = project.in(file("doc-tool")).
    dependsOn(`dotty-compiler-bootstrapped`, `dotty-compiler-bootstrapped` % "test->test").
    settings(commonBootstrappedSettings).
    settings(dottyDocSettings)


  lazy val `dotty-bot` = project.in(file("bot")).
    settings(commonScala2Settings).
    settings(
      resourceDirectory in Test := baseDirectory.value / "test" / "resources",

      // specify main and ignore tests when assembling
      mainClass in assembly := Some("dotty.tools.bot.Main"),
      test in assembly := {},

      libraryDependencies ++= {
        val circeVersion = "0.7.0"
        val http4sVersion = "0.15.3"
        Seq(
          "com.novocode" % "junit-interface" % "0.11" % "test",
          "io.circe" %% "circe-generic" % circeVersion,
          "io.circe" %% "circe-parser" % circeVersion,
          "ch.qos.logback" % "logback-classic" % "1.1.7",
          "org.http4s" %% "http4s-dsl" % http4sVersion,
          "org.http4s" %% "http4s-blaze-server" % http4sVersion,
          "org.http4s" %% "http4s-blaze-client" % http4sVersion,
          "org.http4s" %% "http4s-circe" % http4sVersion
        )
      }
    )

  // Settings shared between dotty-compiler and dotty-compiler-bootstrapped
  lazy val dottyCompilerSettings = Seq(

      // The scala-backend folder is a git submodule that contains a fork of the Scala 2.11
      // compiler developed at https://github.com/lampepfl/scala/tree/sharing-backend.
      // We do not compile the whole submodule, only the part of the Scala 2.11 GenBCode backend
      // that we reuse for dotty.
      // See http://dotty.epfl.ch/docs/contributing/backend.html for more information.
      //
      // NOTE: We link (or copy if symbolic links are not supported) these sources in
      // the current project using `sourceGenerators` instead of simply
      // referencing them using `unmanagedSourceDirectories` because the latter
      // breaks some IDEs.
      sourceGenerators in Compile += Def.task {
        val outputDir = (sourceManaged in Compile).value

        val submoduleCompilerDir = baseDirectory.value / ".." / "scala-backend" / "src" / "compiler"
        val backendDir = submoduleCompilerDir / "scala" / "tools" / "nsc" / "backend"
        val allScalaFiles = GlobFilter("*.scala")

        // NOTE: Keep these exclusions synchronized with the ones in the tests (CompilationTests.scala)
        val files = ((backendDir *
          (allScalaFiles - "JavaPlatform.scala" - "Platform.scala" - "ScalaPrimitives.scala")) +++
         (backendDir / "jvm") *
          (allScalaFiles - "BCodeICodeCommon.scala" - "GenASM.scala" - "GenBCode.scala" - "ScalacBackendInterface.scala" - "BackendStats.scala")
        ).get

        val pairs = files.pair(sbt.Path.rebase(submoduleCompilerDir, outputDir))

        try {
          pairs.foreach { case (src, dst) =>
            sbt.IO.createDirectory(dst.getParentFile)
            if (!dst.exists)
              Files.createSymbolicLink(/*link = */ dst.toPath, /*existing = */src.toPath)
          }
        } catch {
          case _: UnsupportedOperationException | _: FileSystemException =>
            // If the OS doesn't support symbolic links, copy the directory instead.
            sbt.IO.copy(pairs, overwrite = true, preserveLastModified = true)
        }

        pairs.map(_._2)
      }.taskValue,

      // Used by the backend
      libraryDependencies += "org.scala-lang.modules" % "scala-asm" % "5.1.0-scala-2",

      // set system in/out for repl
      connectInput in run := true,
      outputStrategy := Some(StdoutOutput),

      // Generate compiler.properties, used by sbt
      resourceGenerators in Compile += Def.task {
        import java.util._
        import java.text._
        val file = (resourceManaged in Compile).value / "compiler.properties"
        val dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss")
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"))
        val contents =                //2.11.11.v20170413-090219-8a413ba7cc
          s"""version.number=${version.value}
             |maven.version.number=${version.value}
             |git.hash=${VersionUtil.gitHash}
             |copyright.string=Copyright 2002-${Calendar.getInstance().get(Calendar.YEAR)}, LAMP/EPFL
           """.stripMargin

        if (!(file.exists && IO.read(file) == contents)) {
          IO.write(file, contents)
        }

        Seq(file)
      }.taskValue,

      // include sources in eclipse (downloads source code for all dependencies)
      //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
      com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

      // get libraries onboard
      libraryDependencies ++= Seq("com.typesafe.sbt" % "sbt-interface" % sbtVersion.value,
                                  ("org.scala-lang.modules" %% "scala-xml" % "1.0.1").withDottyCompat(),
                                  "com.novocode" % "junit-interface" % "0.11" % "test",
                                  "org.scala-lang" % "scala-library" % scalacVersion % "test"),

      // enable improved incremental compilation algorithm
      incOptions := incOptions.value.withNameHashing(true),

      // For convenience, change the baseDirectory when running the compiler
      baseDirectory in (Compile, run) := baseDirectory.value / "..",
      // .. but not when running test
      baseDirectory in (Test, run) := baseDirectory.value,

      repl := Def.inputTaskDyn {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val dottyLib = packageAll.value("dotty-library")
        (runMain in Compile).toTask(
          s" dotty.tools.dotc.repl.Main -classpath $dottyLib " + args.mkString(" ")
        )
      }.evaluated,

      test in Test := {
        // Exclude legacy tests by default
        (testOnly in Test).toTask(" -- --exclude-categories=java.lang.Exception").value
      },

      vulpix := Def.inputTaskDyn {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val cmd = " dotty.tools.dotc.CompilationTests" + {
          if (args.nonEmpty) " -- -Ddotty.tests.filter=" + args.mkString(" ")
          else ""
        }
        (testOnly in Test).toTask(cmd)
      }.evaluated,

      // Override run to be able to run compiled classfiles
      dotr := {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val java: String = Process("which" :: "java" :: Nil).!!
        val attList = (dependencyClasspath in Runtime).value
        val _  = packageAll.value
        val scalaLib = attList
          .map(_.data.getAbsolutePath)
          .find(_.contains("scala-library"))
          .toList.mkString(":")

        if (java == "")
          println("Couldn't find java executable on path, please install java to a default location")
        else if (scalaLib == "") {
          println("Couldn't find scala-library on classpath, please run using script in bin dir instead")
        } else {
          val dottyLib = packageAll.value("dotty-library")
          s"""$java -classpath .:$dottyLib:$scalaLib ${args.mkString(" ")}""".!
        }
      },
      run := Def.inputTaskDyn {
        val dottyLib = packageAll.value("dotty-library")
        val args: Seq[String] = spaceDelimited("<arg>").parsed

        val fullArgs = args.span(_ != "-classpath") match {
          case (beforeCp, Nil) => beforeCp ++ ("-classpath" :: dottyLib :: Nil)
          case (beforeCp, rest) => beforeCp ++ rest
        }

        (runMain in Compile).toTask(
          s" dotty.tools.dotc.Main " + fullArgs.mkString(" ")
        )
      }.evaluated,
      dotc := run.evaluated,

      // enable verbose exception messages for JUnit
      testOptions in Test += Tests.Argument(
        TestFrameworks.JUnit, "-a", "-v",
        "--run-listener=dotty.tools.ContextEscapeDetector"
      ),

      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      //TODO: disabling until moved to separate project
      //ivyConfigurations += config("sourcedeps").hide,
      //libraryDependencies +=
      //  "org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps",
      //sourceGenerators in Compile += Def.task {
      //  val s = streams.value
      //  val cacheDir = s.cacheDirectory
      //  val trgDir = (sourceManaged in Compile).value / "scalajs-ir-src"

      //  val report = updateClassifiers.value
      //  val scalaJSIRSourcesJar = report.select(
      //      configuration = Set("sourcedeps"),
      //      module = (_: ModuleID).name.startsWith("scalajs-ir_"),
      //      artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
      //    sys.error(s"Could not fetch scalajs-ir sources")
      //  }

      //  FileFunction.cached(cacheDir / s"fetchScalaJSIRSource",
      //      FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
      //    s.log.info(s"Unpacking scalajs-ir sources to $trgDir...")
      //    if (trgDir.exists)
      //      IO.delete(trgDir)
      //    IO.createDirectory(trgDir)
      //    IO.unzip(scalaJSIRSourcesJar, trgDir)
      //    (trgDir ** "*.scala").get.toSet
      //  } (Set(scalaJSIRSourcesJar)).toSeq
      //}.taskValue,

      // Spawn new JVM in run and test
      fork in run := true,
      fork in Test := true,
      parallelExecution in Test := false,

      // Add git-hash used to package the distribution to the manifest to know it in runtime and report it in REPL
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)),

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      // packageAll should always be run before tests
      javaOptions ++= {
        val attList = (dependencyClasspath in Runtime).value
        val pA = packageAll.value

        // put needed dependencies on classpath:
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
          // FIXME: when we snip the cord, this should go bye-bye
          if path.contains("scala-library") ||
            // FIXME: currently needed for tests referencing scalac internals
            path.contains("scala-reflect") ||
            // FIXME: should go away when xml literal parsing is removed
            path.contains("scala-xml") ||
            // used for tests that compile dotty
            path.contains("scala-asm") ||
            // needed for the xsbti interface
            path.contains("sbt-interface")
        } yield "-Xbootclasspath/p:" + path

        val ci_build = // propagate if this is a ci build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}") ::: jenkinsMemLimit
          else if (sys.props.isDefinedAt(DRONE_MEM))
            List("-Xmx" + sys.props(DRONE_MEM))
          else List()

        val tuning =
          if (sys.props.isDefinedAt("Oshort"))
            // Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
            List("-XX:+TieredCompilation", "-XX:TieredStopAtLevel=1")
          else List()

        val jars = List(
          "-Ddotty.tests.classes.interfaces=" + pA("dotty-interfaces"),
          "-Ddotty.tests.classes.library=" + pA("dotty-library"),
          "-Ddotty.tests.classes.compiler=" + pA("dotty-compiler")
        )

        jars ::: tuning ::: agentOptions ::: ci_build ::: path.toList
      }
  )

  lazy val `dotty-compiler` = project.in(file("compiler")).
    dependsOn(`dotty-interfaces`).
    dependsOn(`dotty-library`).
    settings(commonNonBootstrappedSettings).
    settings(dottyCompilerSettings).
    settings(
      // Disable scaladoc generation, it's way too slow and we'll replace it
      // by dottydoc anyway. We still publish an empty -javadoc.jar to make
      // sonatype happy.
      sources in (Compile, doc) := Seq(),

      // packageAll packages all and then returns a map with the abs location
      packageAll := {
        Map(
          "dotty-interfaces" -> (packageBin in (`dotty-interfaces`, Compile)).value,
          "dotty-compiler" -> (packageBin in Compile).value,
          "dotty-library" -> (packageBin in (`dotty-library`, Compile)).value,
          "dotty-compiler-test" -> (packageBin in Test).value
        ) map { case (k, v) => (k, v.getAbsolutePath) }
      }
    )

  lazy val `dotty-compiler-bootstrapped` = project.in(file("compiler")).
    dependsOn(`dotty-interfaces`).
    dependsOn(`dotty-library-bootstrapped`).
    settings(commonBootstrappedSettings).
    settings(dottyCompilerSettings).
    settings(
      packageAll := {
        (packageAll in `dotty-compiler`).value ++ Seq(
          ("dotty-compiler" -> (packageBin in Compile).value.getAbsolutePath),
          ("dotty-library" -> (packageBin in (`dotty-library-bootstrapped`, Compile)).value.getAbsolutePath)
        )
      }
    )

  /* Contains unit tests for the scripts */
  lazy val `dotty-bin-tests` = project.in(file("bin")).
    settings(commonNonBootstrappedSettings).
    settings(
      publishArtifact := false,
      parallelExecution in Test := false,
      // Increase verbosity of test output, started and passed tests are
      // logged with:
      testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.11" % "test"
    )

  // Settings shared between dotty-library and dotty-library-bootstrapped
  lazy val dottyLibrarySettings = Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-library" % scalacVersion,
        "com.novocode" % "junit-interface" % "0.11" % "test"
      )
  )

  lazy val `dotty-library` = project.in(file("library")).
    settings(commonNonBootstrappedSettings).
    settings(dottyLibrarySettings)

  lazy val `dotty-library-bootstrapped`: Project = project.in(file("library")).
    settings(commonBootstrappedSettings).
    settings(dottyLibrarySettings).
    settings(
      // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called.
      scalacOptions in Compile ++= Seq("-sourcepath", (scalaSource in Compile).value.getAbsolutePath)
    )

  // until sbt/sbt#2402 is fixed (https://github.com/sbt/sbt/issues/2402)
  lazy val cleanSbtBridge = TaskKey[Unit]("cleanSbtBridge", "delete dotty-sbt-bridge cache")

  lazy val dottySbtBridgeSettings = Seq(
    cleanSbtBridge := {
      val dottySbtBridgeVersion = version.value
      val dottyVersion = (version in `dotty-compiler`).value
      val classVersion = System.getProperty("java.class.version")

      val sbtV = sbtVersion.value
      val sbtOrg = "org.scala-sbt"
      val sbtScalaVersion = "2.10.6"

      val home = System.getProperty("user.home")
      val org = organization.value
      val artifact = moduleName.value

      IO.delete(file(home) / ".ivy2" / "cache" / sbtOrg / s"$org-$artifact-$dottySbtBridgeVersion-bin_${dottyVersion}__$classVersion")
      IO.delete(file(home) / ".sbt"  / "boot" / s"scala-$sbtScalaVersion" / sbtOrg / "sbt" / sbtV / s"$org-$artifact-$dottySbtBridgeVersion-bin_${dottyVersion}__$classVersion")
    },
    packageSrc in Compile := (packageSrc in Compile).dependsOn(cleanSbtBridge).value,
    description := "sbt compiler bridge for Dotty",
    resolvers += Resolver.typesafeIvyRepo("releases"), // For org.scala-sbt:api
    libraryDependencies ++= Seq(
      "com.typesafe.sbt" % "sbt-interface" % sbtVersion.value,
      "org.scala-sbt" % "api" % sbtVersion.value % "test",
      ("org.specs2" %% "specs2" % "2.3.11" % "test").withDottyCompat()
    ),
    // The sources should be published with crossPaths := false since they
    // need to be compiled by the project using the bridge.
    crossPaths := false,

    // Don't publish any binaries for the bridge because of the above
    publishArtifact in (Compile, packageBin) := false,

    fork in Test := true,
    parallelExecution in Test := false
  )

  lazy val `dotty-sbt-bridge` = project.in(file("sbt-bridge")).
    dependsOn(`dotty-compiler`).
    settings(commonNonBootstrappedSettings).
    settings(dottySbtBridgeSettings)

  lazy val `dotty-sbt-bridge-bootstrapped` = project.in(file("sbt-bridge")).
    dependsOn(`dotty-compiler-bootstrapped`).
    settings(commonBootstrappedSettings).
    settings(dottySbtBridgeSettings).
    settings(
      // Disabled because dotty crashes when compiling the tests
      sources in Test := Seq()
    )

  lazy val `dotty-language-server` = project.in(file("language-server")).
    dependsOn(`dotty-compiler-bootstrapped`).
    settings(commonBootstrappedSettings).
    settings(
      // Sources representing the shared configuration file used to communicate between the sbt-dotty
      // plugin and the language server
      unmanagedSourceDirectories in Compile += baseDirectory.value / "../sbt-dotty/src/dotty/tools/sbtplugin/config",

      // fork so that the shutdown hook in Main is run when we ctrl+c a run
      // (you need to have `cancelable in Global := true` in your global sbt config to ctrl+c a run)
      fork in run := true,
      libraryDependencies ++= Seq(
        "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.2.0",
        Dependencies.`jackson-databind`
      ),
      javaOptions := (javaOptions in `dotty-compiler-bootstrapped`).value,

      run := Def.inputTaskDyn {
        val inputArgs = spaceDelimited("<arg>").parsed

        val mainClass = "dotty.tools.languageserver.Main"
        val extensionPath = (baseDirectory in `vscode-dotty`).value.getAbsolutePath

        val codeArgs =
          s"--extensionDevelopmentPath=$extensionPath" +:
            (if (inputArgs.isEmpty) List((baseDirectory.value / "..").getAbsolutePath) else inputArgs)

        val clientCommand = prepareCommand(codeCommand.value ++ codeArgs)

        val allArgs = "-client_command" +: clientCommand

        runTask(Runtime, mainClass, allArgs: _*)
      }.dependsOn(compile in (`vscode-dotty`, Compile)).evaluated
    )

  /** A sandbox to play with the Scala.js back-end of dotty.
   *
   *  This sandbox is compiled with dotty with support for Scala.js. It can be
   *  used like any regular Scala.js project. In particular, `fastOptJS` will
   *  produce a .js file, and `run` will run the JavaScript code with a JS VM.
   *
   *  Simply running `dotty/run -scalajs` without this sandbox is not very
   *  useful, as that would not provide the linker and JS runners.
   */
  lazy val sjsSandbox = project.in(file("sandbox/scalajs")).
    enablePlugins(ScalaJSPlugin).
    settings(commonNonBootstrappedSettings).
    settings(
      /* Remove the Scala.js compiler plugin for scalac, and enable the
       * Scala.js back-end of dotty instead.
       */
      libraryDependencies ~= { deps =>
        deps.filterNot(_.name.startsWith("scalajs-compiler"))
      },
      scalacOptions += "-scalajs",

      // The main class cannot be found automatically due to the empty inc.Analysis
      mainClass in Compile := Some("hello.world"),

      // While developing the Scala.js back-end, it is very useful to see the trees dotc gives us
      scalacOptions += "-Xprint:labelDef",

      /* Debug-friendly Scala.js optimizer options.
       * In particular, typecheck the Scala.js IR found on the classpath.
       */
      scalaJSOptimizerOptions ~= {
        _.withCheckScalaJSIR(true).withParallel(false)
      }
    ).
    settings(compileWithDottySettings).
    settings(inConfig(Compile)(Seq(
      /* Make sure jsDependencyManifest runs after compile, otherwise compile
       * might remove the entire directory afterwards.
       */
      jsDependencyManifest := jsDependencyManifest.dependsOn(compile).value
    )))

  lazy val `dotty-bench` = project.in(file("bench")).
    dependsOn(`dotty-compiler` % "compile->test").
    settings(commonNonBootstrappedSettings).
    settings(
      baseDirectory in (Test,run) := (baseDirectory in `dotty-compiler`).value,

      libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6" % Test,

      fork in Test := true,
      parallelExecution in Test := false,

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      javaOptions ++= {
        val attList = (dependencyClasspath in Runtime).value
        val bin = (packageBin in Compile).value

        // put the Scala {library, reflect, compiler} in the classpath
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
          prefix = if (path.endsWith(".jar")) "p" else "a"
        } yield "-Xbootclasspath/" + prefix + ":" + path
        // dotty itself needs to be in the bootclasspath
        val fullpath = ("-Xbootclasspath/a:" + bin) :: path.toList
        // System.err.println("BOOTPATH: " + fullpath)

        val ci_build = // propagate if this is a ci build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}")
          else if (sys.props.isDefinedAt(DRONE_MEM))
            List("-Xmx" + sys.props(DRONE_MEM))
          else
            List()
        val res = agentOptions ::: ci_build ::: fullpath
        println("Running with javaOptions: " + res)
        res
      }
    )

  // Depend on dotty-library so that sbt projects using dotty automatically
  // depend on the dotty-library
  lazy val `scala-library` = project.
    dependsOn(`dotty-library-bootstrapped`).
    settings(commonDummySettings).
    settings(
      // Need a direct dependency on the real scala-library even though we indirectly
      // depend on it via dotty-library, because sbt may rewrite dependencies
      // (see https://github.com/sbt/sbt/pull/2634), but won't rewrite the direct
      // dependencies of scala-library (see https://github.com/sbt/sbt/pull/2897)
      libraryDependencies += "org.scala-lang" % "scala-library" % scalacVersion
    )

  lazy val `scala-compiler` = project.
    settings(commonDummySettings)
  lazy val `scala-reflect` = project.
    settings(commonDummySettings).
    settings(
      libraryDependencies := Seq("org.scala-lang" % "scala-reflect" % scalacVersion)
    )
  lazy val scalap = project.
    settings(commonDummySettings).
    settings(
      libraryDependencies := Seq("org.scala-lang" % "scalap" % scalacVersion)
    )


  // sbt plugin to use Dotty in your own build, see
  // https://github.com/lampepfl/dotty-example-project for usage.
  lazy val `sbt-dotty` = project.in(file("sbt-dotty")).
    settings(commonSettings).
    settings(
      // Keep in sync with inject-sbt-dotty.sbt
      libraryDependencies += Dependencies.`jackson-databind`,
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config",


      sbtPlugin := true,
      version := "0.1.3",
      ScriptedPlugin.scriptedSettings,
      ScriptedPlugin.sbtTestDirectory := baseDirectory.value / "sbt-test",
      ScriptedPlugin.scriptedBufferLog := false,
      ScriptedPlugin.scriptedLaunchOpts += "-Dplugin.version=" + version.value,
      ScriptedPlugin.scriptedLaunchOpts += "-Dplugin.scalaVersion=" + dottyVersion,
      ScriptedPlugin.scripted := ScriptedPlugin.scripted.dependsOn(Def.task {
        val x0 = (publishLocal in `dotty-sbt-bridge-bootstrapped`).value
        val x1 = (publishLocal in `dotty-interfaces`).value
        val x2 = (publishLocal in `dotty-compiler-bootstrapped`).value
        val x3 = (publishLocal in `dotty-library-bootstrapped`).value
        val x4 = (publishLocal in `scala-library`).value
        val x5 = (publishLocal in `scala-reflect`).value
        val x6 = (publishLocal in `dotty-bootstrapped`).value // Needed because sbt currently hardcodes the dotty artifact
      }).evaluated
    )

  lazy val `vscode-dotty` = project.in(file("vscode-dotty")).
    settings(commonSettings).
    settings(
      EclipseKeys.skipProject := true,

      version := "0.1.0", // Keep in sync with package.json

      autoScalaLibrary := false,
      publishArtifact := false,
      includeFilter in unmanagedSources := NothingFilter | "*.ts" | "**.json",
      watchSources in Global ++= (unmanagedSources in Compile).value,
      compile in Compile := {
        val coursier = baseDirectory.value / "out/coursier"
        val packageJson = baseDirectory.value / "package.json"
        if (!coursier.exists || packageJson.lastModified > coursier.lastModified)
          runProcess(Seq("npm", "run", "update-all"), wait = true, directory = baseDirectory.value)
        val tsc = baseDirectory.value / "node_modules" / ".bin" / "tsc"
        runProcess(Seq(tsc.getAbsolutePath, "--pretty", "--project", baseDirectory.value.getAbsolutePath), wait = true)

        // Currently, vscode-dotty depends on daltonjorge.scala for syntax highlighting,
        // this is not automatically installed when starting the extension in development mode
        // (--extensionDevelopmentPath=...)
        runProcess(codeCommand.value ++ Seq("--install-extension", "daltonjorge.scala"), wait = true)

        sbt.inc.Analysis.Empty
      },
      sbt.Keys.`package`:= {
        runProcess(Seq("vsce", "package"), wait = true, directory = baseDirectory.value)

        baseDirectory.value / s"dotty-${version.value}.vsix"
      },
      unpublish := {
        runProcess(Seq("vsce", "unpublish"), wait = true, directory = baseDirectory.value)
      },
      publish := {
        runProcess(Seq("vsce", "publish"), wait = true, directory = baseDirectory.value)
      },
      run := Def.inputTask {
        val inputArgs = spaceDelimited("<arg>").parsed
        val codeArgs = if (inputArgs.isEmpty) List((baseDirectory.value / "..").getAbsolutePath) else inputArgs
        val extensionPath = baseDirectory.value.getAbsolutePath
        val processArgs = List(s"--extensionDevelopmentPath=${extensionPath}") ++ codeArgs

        runProcess(codeCommand.value ++ processArgs, wait = true)
      }.dependsOn(compile in Compile).evaluated
    )


   lazy val publishSettings = Seq(
     publishMavenStyle := true,
     isSnapshot := version.value.contains("SNAPSHOT"),
     publishTo := {
       val nexus = "https://oss.sonatype.org/"
       if (isSnapshot.value)
         Some("snapshots" at nexus + "content/repositories/snapshots")
       else
         Some("releases"  at nexus + "service/local/staging/deploy/maven2")
     },
     publishArtifact in Test := false,
     homepage := Some(url(dottyGithubUrl)),
     licenses += ("BSD New",
       url(s"$dottyGithubUrl/blob/master/LICENSE.md")),
     scmInfo := Some(
       ScmInfo(
         url(dottyGithubUrl),
         "scm:git:git@github.com:lampepfl/dotty.git"
       )
     ),
     pomExtra := (
       <developers>
         <developer>
           <id>odersky</id>
           <name>Martin Odersky</name>
           <email>martin.odersky@epfl.ch</email>
           <url>https://github.com/odersky</url>
         </developer>
         <developer>
           <id>DarkDimius</id>
           <name>Dmitry Petrashko</name>
           <email>me@d-d.me</email>
           <url>https://d-d.me</url>
         </developer>
         <developer>
           <id>smarter</id>
           <name>Guillaume Martres</name>
           <email>smarter@ubuntu.com</email>
           <url>http://guillaume.martres.me</url>
         </developer>
         <developer>
           <id>felixmulder</id>
           <name>Felix Mulder</name>
           <email>felix.mulder@gmail.com</email>
           <url>http://felixmulder.com</url>
         </developer>
         <developer>
           <id>liufengyun</id>
           <name>Liu Fengyun</name>
           <email>liufengyun@chaos-lab.com</email>
           <url>http://chaos-lab.com</url>
         </developer>
       </developers>
     )
   )

  // Compile with dotty
  lazy val compileWithDottySettings = {
    inConfig(Compile)(inTask(compile)(Defaults.runnerTask) ++ Seq(
      // Compile with dotty
      fork in compile := true,

      compile := {
        val inputs = (compileInputs in compile).value
        import inputs.config._

        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory

        // Discover classpaths

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

        val compilerCp = Attributed.data((fullClasspath in (`dotty-compiler`, Compile)).value)
        val cpStr = cpToString(classpath ++ compilerCp)

        // List all my dependencies (recompile if any of these changes)

        val allMyDependencies = classpath filterNot (_ == classesDirectory) flatMap { cpFile =>
          if (cpFile.isDirectory) (cpFile ** "*.class").get
          else Seq(cpFile)
        }

        // Compile

        val cachedCompile = FileFunction.cached(cacheDir / "compile",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

          logger.info(
              "Compiling %d Scala sources to %s..." format (
              sources.size, classesDirectory))

          if (classesDirectory.exists)
            IO.delete(classesDirectory)
          IO.createDirectory(classesDirectory)

          val sourcesArgs = sources.map(_.getAbsolutePath()).toList

          /* run.run() below in doCompile() will emit a call to its
           * logger.info("Running dotty.tools.dotc.Main [...]")
           * which we do not want to see. We use this patched logger to
           * filter out that particular message.
           */
          val patchedLogger = new Logger {
            def log(level: Level.Value, message: => String) = {
              val msg = message
              if (level != Level.Info ||
                  !msg.startsWith("Running dotty.tools.dotc.Main"))
                logger.log(level, msg)
            }
            def success(message: => String) = logger.success(message)
            def trace(t: => Throwable) = logger.trace(t)
          }

          def doCompile(sourcesArgs: List[String]): Unit = {
            val run = (runner in compile).value
            run.run("dotty.tools.dotc.Main", compilerCp,
                "-classpath" :: cpStr ::
                "-d" :: classesDirectory.getAbsolutePath() ::
                options ++:
                sourcesArgs,
                patchedLogger) foreach sys.error
          }

          // Work around the Windows limitation on command line length.
          val isWindows =
            System.getProperty("os.name").toLowerCase().indexOf("win") >= 0
          if ((fork in compile).value && isWindows &&
              (sourcesArgs.map(_.length).sum > 1536)) {
            IO.withTemporaryFile("sourcesargs", ".txt") { sourceListFile =>
              IO.writeLines(sourceListFile, sourcesArgs)
              doCompile(List("@"+sourceListFile.getAbsolutePath()))
            }
          } else {
            doCompile(sourcesArgs)
          }

          // Output is all files in classesDirectory
          (classesDirectory ** AllPassFilter).get.toSet
        }

        cachedCompile((sources ++ allMyDependencies).toSet)

        // We do not have dependency analysis when compiling externally
        sbt.inc.Analysis.Empty
      }
    ))
  }

  lazy val submoduleChecks = onLoad in Global := (onLoad in Global).value andThen { state =>
    val submodules = List(new File("scala-backend"), new File("scala2-library"), new File("collection-strawman"))
    if (!submodules.forall(f => f.exists && f.listFiles().nonEmpty)) {
      sLog.value.log(Level.Error,
        s"""Missing some of the submodules
           |You can initialize the modules with:
           |  > git submodule update --init
        """.stripMargin)
    }
    state
  }

  lazy val dist = project.
    dependsOn(`dotty-interfaces`).
    dependsOn(`dotty-compiler`).
    dependsOn(`dotty-library`).
    dependsOn(`dotty-doc`).
    settings(commonNonBootstrappedSettings).
    settings(packSettings).
    settings(
      publishArtifact := false,
      // packMain := Map("dummy" -> "dotty.tools.dotc.Main"),
      packExpandedClasspath := true,
      packResourceDir += (baseDirectory.value / "bin" -> "bin"),
      packArchiveName := "dotty-" + dottyVersion
    )

   // Same as `dist` but using bootstrapped projects.
  lazy val `dist-bootstrapped` = project.
    dependsOn(`dotty-interfaces`).
    dependsOn(`dotty-library-bootstrapped`).
    dependsOn(`dotty-compiler-bootstrapped`).
    dependsOn(`dotty-doc-bootstrapped`).
    settings(commonBootstrappedSettings).
    settings(packSettings).
    settings(
      target := baseDirectory.value / "target",                    // override setting in commonBootstrappedSettings
      publishArtifact := false,
      // packMain := Map("dummy" -> "dotty.tools.dotc.Main"),
      packExpandedClasspath := true,
      // packExcludeJars := Seq("scala-library-.*\\.jar"),
      packResourceDir += (baseDirectory.value / "bin" -> "bin"),
      packArchiveName := "dotty-" + dottyVersion
    )
}
