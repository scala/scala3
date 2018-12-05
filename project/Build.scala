import java.io.File
import java.nio.file._

import Modes._
import com.typesafe.sbt.pgp.PgpKeys
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import sbt.Keys._
import sbt._
import complete.DefaultParsers._
import pl.project13.scala.sbt.JmhPlugin
import pl.project13.scala.sbt.JmhPlugin.JmhKeys.Jmh
import sbt.Package.ManifestAttributes
import sbt.plugins.SbtPlugin
import sbt.ScriptedPlugin.autoImport._
import xerial.sbt.pack.PackPlugin
import xerial.sbt.pack.PackPlugin.autoImport._

import dotty.tools.sbtplugin.DottyPlugin.autoImport._
import dotty.tools.sbtplugin.DottyIDEPlugin.{ installCodeExtension, prepareCommand, runProcess }
import dotty.tools.sbtplugin.DottyIDEPlugin.autoImport._

import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._

import scala.util.Properties.isJavaAtLeast

/* In sbt 0.13 the Build trait would expose all vals to the shell, where you
 * can use them in "set a := b" like expressions. This re-exposes them.
 */
object ExposedValues extends AutoPlugin {
  object autoImport {
    val bootstrapFromPublishedJars = Build.bootstrapFromPublishedJars
  }
}

object Build {
  val scalacVersion = "2.12.7"

  val baseVersion = "0.12.0"
  val baseSbtDottyVersion = "0.2.7"

  // Versions used by the vscode extension to create a new project
  // This should be the latest published releases.
  // TODO: Have the vscode extension fetch these numbers from the Internet
  // instead of hardcoding them ?
  val publishedDottyVersion = "0.11.0-bin-20181101-714ce80-NIGHTLY" // Using a nightly for now to get worksheet support
  val publishedSbtDottyVersion = "0.2.6"


  val dottyOrganization = "ch.epfl.lamp"
  val dottyGithubUrl = "https://github.com/lampepfl/dotty"


  val isRelease = sys.env.get("RELEASEBUILD") == Some("yes")

  val dottyVersion = {
    def isNightly = sys.env.get("NIGHTLYBUILD") == Some("yes")
    if (isRelease)
      baseVersion
    else if (isNightly)
      baseVersion + "-bin-" + VersionUtil.commitDate + "-" + VersionUtil.gitHash + "-NIGHTLY"
    else
      baseVersion + "-bin-SNAPSHOT"
  }
  val dottyNonBootstrappedVersion = dottyVersion + "-nonbootstrapped"

  val sbtDottyName = "sbt-dotty"
  val sbtDottyVersion = {
    if (isRelease) baseSbtDottyVersion else baseSbtDottyVersion + "-SNAPSHOT"
  }

  val agentOptions = List(
    // "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
    // "-agentpath:/home/dark/opt/yjp-2013-build-13072/bin/linux-x86-64/libyjpagent.so"
    // "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15052.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
    // "-XX:+HeapDumpOnOutOfMemoryError", "-Xmx1g", "-Xss2m"
  )

  // Packages all subprojects to their jars
  lazy val packageAll =
    taskKey[Map[String, String]]("Package everything needed to run tests")

  // Run tests with filter through vulpix test suite
  lazy val testCompilation = inputKey[Unit]("runs integration test with the supplied filter")

  // Run TASTY tests with filter through vulpix test suite
  lazy val testFromTasty = inputKey[Unit]("runs tasty integration test with the supplied filter")

  // Spawns a repl with the correct classpath
  lazy val repl = inputKey[Unit]("run the REPL with correct classpath")

  // Used to compile files similar to ./bin/dotc script
  lazy val dotc =
    inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

  // Used to run binaries similar to ./bin/dotr script
  lazy val dotr =
    inputKey[Unit]("run compiled binary using the correct classpath, or the user supplied classpath")


  // Compiles the documentation and static site
  lazy val genDocs = taskKey[Unit]("run dottydoc to generate static documentation site")

  // Shorthand for compiling a docs site
  lazy val dottydoc = inputKey[Unit]("run dottydoc")

  lazy val bootstrapFromPublishedJars = settingKey[Boolean]("If true, bootstrap dotty from published non-bootstrapped dotty")

  // Only available in vscode-dotty
  lazy val unpublish = taskKey[Unit]("Unpublish a package")

  // Settings used to configure the test language server
  lazy val ideTestsCompilerVersion = taskKey[String]("Compiler version to use in IDE tests")
  lazy val ideTestsCompilerArguments = taskKey[Seq[String]]("Compiler arguments to use in IDE tests")
  lazy val ideTestsDependencyClasspath = taskKey[Seq[File]]("Dependency classpath to use in IDE tests")

  // Settings shared by the build (scoped in ThisBuild). Used in build.sbt
  lazy val thisBuildSettings = Def.settings(
    organization := dottyOrganization,
    organizationName := "LAMP/EPFL",
    organizationHomepage := Some(url("http://lamp.epfl.ch")),

    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-Xfatal-warnings",
      "-encoding", "UTF8",
      "-language:existentials,higherKinds,implicitConversions"
    ),

    javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    // Change this to true if you want to bootstrap using a published non-bootstrapped compiler
    bootstrapFromPublishedJars := false,

    // Override `runCode` from sbt-dotty to use the language-server and
    // vscode extension from the source repository of dotty instead of a
    // published version.
    runCode := (run in `dotty-language-server`).toTask("").value,

    // include sources in eclipse (downloads source code for all dependencies)
    //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
    EclipseKeys.withSource := true
  )

  // Settings shared globally (scoped in Global). Used in build.sbt
  lazy val globalSettings = Def.settings(
    onLoad := (onLoad in Global).value andThen { state =>
      def exists(submodule: String) = {
        val path = Paths.get(submodule)
        Files.exists(path) && {
          val fileStream = Files.list(path)
          try fileStream.iterator().hasNext
          finally fileStream.close()
        }
      }

      // Make sure all submodules are properly cloned
      val submodules = List("scala-backend", "scala2-library", "collection-strawman")
      if (!submodules.forall(exists)) {
        sLog.value.log(Level.Error,
          s"""Missing some of the submodules
             |You can initialize the modules with:
             |  > git submodule update --init
          """.stripMargin)
      }

      // Copy default configuration from .vscode-template/ unless configuration files already exist in .vscode/
      sbt.IO.copyDirectory(new File(".vscode-template/"), new File(".vscode/"), overwrite = false)

      state
    },

    // Credentials to release to Sonatype
    credentials ++= (
      for {
        username <- sys.env.get("SONATYPE_USER")
        password <- sys.env.get("SONATYPE_PW")
      } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
    ).toList,
    PgpKeys.pgpPassphrase := sys.env.get("PGP_PW").map(_.toCharArray())
  )

  lazy val commonSettings = publishSettings ++ Seq(
    scalaSource       in Compile    := baseDirectory.value / "src",
    scalaSource       in Test       := baseDirectory.value / "test",
    javaSource        in Compile    := baseDirectory.value / "src",
    javaSource        in Test       := baseDirectory.value / "test",
    resourceDirectory in Compile    := baseDirectory.value / "resources",
    resourceDirectory in Test       := baseDirectory.value / "test-resources",

    // Disable scaladoc generation, it's way too slow and we'll replace it
    // by dottydoc anyway. We still publish an empty -javadoc.jar to make
    // sonatype happy.
    sources in (Compile, doc) := Seq(),

    // Prevent sbt from rewriting our dependencies
    scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(false))),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,

    // enable verbose exception messages for JUnit
    testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")
  )

  // Settings used for projects compiled only with Scala 2
  lazy val commonScala2Settings = commonSettings ++ Seq(
    version := dottyVersion,
    scalaVersion := scalacVersion
  )

  // Settings used when compiling dotty using Scala 2
  lazy val commonNonBootstrappedSettings = commonSettings ++ Seq(
    version := dottyNonBootstrappedVersion,
    scalaVersion := scalacVersion
  )

  // Settings used when compiling dotty with a non-bootstrapped dotty
  lazy val commonBootstrappedSettings = commonSettings ++ Seq(
    version := dottyVersion,
    scalaVersion := dottyNonBootstrappedVersion,

    // Avoid having to run `dotty-sbt-bridge/publishLocal` before compiling a bootstrapped project
    scalaCompilerBridgeSource :=
      (dottyOrganization %% "dotty-sbt-bridge" % dottyVersion)
      .artifacts(Artifact.sources("dotty-sbt-bridge").withUrl(
        // We cannot use the `packageSrc` task because a setting cannot depend
        // on a task. Instead, we make `compile` below depend on the bridge `packageSrc`
        Some((artifactPath in (`dotty-sbt-bridge`, Compile, packageSrc)).value.toURI.toURL))),
    compile in Compile := (compile in Compile)
      .dependsOn(packageSrc in (`dotty-sbt-bridge`, Compile))
      .dependsOn(compile in (`dotty-sbt-bridge`, Compile))
      .value,

    // Use the same name as the non-bootstrapped projects for the artifacts
    moduleName ~= { _.stripSuffix("-bootstrapped") },

    // Prevent sbt from setting the Scala bootclasspath, otherwise it will
    // contain `scalaInstance.value.libraryJar` which in our case is the
    // non-bootstrapped dotty-library that will then take priority over
    // the bootstrapped dotty-library on the classpath or sourcepath.
    classpathOptions ~= (_.withAutoBoot(false)),
    // ... but when running under Java 8, we still need a Scala bootclasspath that contains the JVM bootclasspath,
    // otherwise sbt incremental compilation breaks.
    scalacOptions ++= {
      if (isJavaAtLeast("9"))
        Seq()
      else
        Seq("-bootclasspath", sys.props("sun.boot.class.path"))
    },

    // Enforce that the only Scala 2 classfiles we unpickle come from scala-library
    /*
    scalacOptions ++= {
      val attList = (dependencyClasspath in `dotty-library` in Compile).value
      val scalaLib = findLib(attList, "scala-library")
      Seq("-Yscala2-unpickler", scalaLib)
    },
    */

    // sbt gets very unhappy if two projects use the same target
    target := baseDirectory.value / ".." / "out" / "bootstrap" / name.value,

    // The non-bootstrapped dotty-library is not necessary when bootstrapping dotty
    autoScalaLibrary := false,
    // ...but scala-library is
    libraryDependencies += "org.scala-lang" % "scala-library" % scalacVersion,

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
        ).map(_.withDottyCompat(scalaVersion.value))
      else
        Seq()
    },

    // Compile using the non-bootstrapped and non-published dotty
    managedScalaInstance := false,
    scalaInstance := {
      import sbt.internal.inc.ScalaInstance
      import sbt.internal.inc.classpath.ClasspathUtilities

      val updateReport = update.value
      var libraryJar = packageBin.in(`dotty-library`, Compile).value
      var compilerJar = packageBin.in(`dotty-compiler`, Compile).value

      if (bootstrapFromPublishedJars.value) {
        val jars = updateReport.select(
            configuration = configurationFilter(Configurations.ScalaTool.name),
            module = moduleFilter(),
            artifact = artifactFilter(extension = "jar")
          )
        libraryJar = jars.find(_.getName.startsWith("dotty-library_2.12")).get
        compilerJar = jars.find(_.getName.startsWith("dotty-compiler_2.12")).get
      }

      // All dotty-doc's and compiler's dependencies except the library.
      // (we get the compiler's dependencies because dottydoc depends on the compiler)
      val otherDependencies = {
        val excluded = Set("dotty-library", "dotty-compiler")
        fullClasspath.in(`dotty-doc`, Compile).value
          .filterNot(_.get(artifact.key).exists(a => excluded.contains(a.name)))
          .map(_.data)
      }

      val allJars = libraryJar :: compilerJar :: otherDependencies.toList
      val classLoader = state.value.classLoaderCache(allJars)
      new ScalaInstance(
        scalaVersion.value,
        classLoader,
        ClasspathUtilities.rootLoader, // FIXME: Should be a class loader which only includes the dotty-lib
                                       // See: https://github.com/sbt/zinc/commit/9397b6aaf94ac3cfab386e3abd11c0ef9c2ceaff#diff-ea135f2f26f43e40ff045089da221e1e
                                       // Should not matter, as it addresses an issue with `sbt run` that
                                       // only occur when `(fork in run) := false`
        libraryJar,
        compilerJar,
        allJars.toArray,
        None
      )
    }
  )

  lazy val commonBenchmarkSettings = Seq(
    outputStrategy := Some(StdoutOutput),
    mainClass in (Jmh, run) := Some("dotty.tools.benchmarks.Bench"), // custom main for jmh:run
    javaOptions += "-DBENCH_COMPILER_CLASS_PATH=" + Attributed.data((fullClasspath in (`dotty-bootstrapped`, Compile)).value).mkString("", File.pathSeparator, ""),
    javaOptions += "-DBENCH_CLASS_PATH=" + Attributed.data((fullClasspath in (`dotty-library-bootstrapped`, Compile)).value).mkString("", File.pathSeparator, "")
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

  def dottySbtBridgeReference(implicit mode: Mode): LocalProject = mode match {
    case NonBootstrapped => dottySbtBridgeRef
    case _ => dottySbtBridgeBootstrappedRef
  }

  // The root project:
  // - aggregates other projects so that "compile", "test", etc are run on all projects at once.
  // - publishes its own empty artifact "dotty" that depends on "dotty-library" and "dotty-compiler",
  //   this is only necessary for compatibility with sbt which currently hardcodes the "dotty" artifact name
  lazy val dotty = project.in(file(".")).asDottyRoot(NonBootstrapped)
  lazy val `dotty-bootstrapped` = project.asDottyRoot(Bootstrapped)

  lazy val `dotty-interfaces` = project.in(file("interfaces")).
    settings(commonScala2Settings). // Java-only project, so this is fine
    settings(
      // Do not append Scala versions to the generated artifacts
      crossPaths := false,
      // Do not depend on the Scala library
      autoScalaLibrary := false,
      //Remove javac invalid options in Compile doc
      javacOptions in (Compile, doc) --= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    )

  private lazy val dottydocClasspath = Def.task {
    val jars = (packageAll in `dotty-compiler`).value
    val dottyLib = jars("dotty-library")
    val dottyInterfaces = jars("dotty-interfaces")
    val otherDeps = (dependencyClasspath in Compile).value.map(_.data).mkString(File.pathSeparator)
    dottyLib + File.pathSeparator + dottyInterfaces + File.pathSeparator + otherDeps
  }

  lazy val semanticdbSettings = Seq(
    baseDirectory in (Compile, run) := baseDirectory.value / "..",
    baseDirectory in Test := baseDirectory.value / "..",
    unmanagedSourceDirectories in Test += baseDirectory.value / "input" / "src" / "main" / "scala",
    libraryDependencies ++= List(
      ("org.scalameta" %% "semanticdb" % "4.0.0").withDottyCompat(scalaVersion.value),
      "com.novocode" % "junit-interface" % "0.11",
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
    )
  )

  // Settings shared between dotty-doc and dotty-doc-bootstrapped
  lazy val dottyDocSettings = Seq(
    baseDirectory in (Compile, run) := baseDirectory.value / "..",
    baseDirectory in Test := baseDirectory.value / "..",

    connectInput in run := true,
    outputStrategy := Some(StdoutOutput),

    javaOptions ++= (javaOptions in `dotty-compiler`).value,
    fork in run := true,
    fork in Test := true,
    parallelExecution in Test := false,

    genDocs := Def.taskDyn {
      // Make majorVersion available at dotty.epfl.ch/versions/latest-nightly-base
      // Used by sbt-dotty to resolve the latest nightly
      val majorVersion = baseVersion.take(baseVersion.lastIndexOf('.'))
      IO.write(file("./docs/_site/versions/latest-nightly-base"), majorVersion)

      val sources =
        unmanagedSources.in(Compile, compile).value ++
        unmanagedSources.in(`dotty-compiler`, Compile).value
      val args = Seq(
        "-siteroot", "docs",
        "-project", "Dotty",
        "-project-version", dottyVersion,
        "-project-url", dottyGithubUrl,
        "-classpath", dottydocClasspath.value
      )
      (runMain in Compile).toTask(
        s""" dotty.tools.dottydoc.Main ${args.mkString(" ")} ${sources.mkString(" ")}"""
      )
    }.value,

    dottydoc := Def.inputTaskDyn {
      val args = spaceDelimited("<arg>").parsed
      val cp = dottydocClasspath.value

      (runMain in Compile).toTask(s" dotty.tools.dottydoc.Main -classpath $cp " + args.mkString(" "))
    }.evaluated,

    libraryDependencies ++= {
      val flexmarkVersion = "0.28.32"
      Seq(
        "com.vladsch.flexmark" % "flexmark" % flexmarkVersion,
        "com.vladsch.flexmark" % "flexmark-ext-gfm-tasklist" % flexmarkVersion,
        "com.vladsch.flexmark" % "flexmark-ext-gfm-tables" % flexmarkVersion,
        "com.vladsch.flexmark" % "flexmark-ext-autolink" % flexmarkVersion,
        "com.vladsch.flexmark" % "flexmark-ext-anchorlink" % flexmarkVersion,
        "com.vladsch.flexmark" % "flexmark-ext-emoji" % flexmarkVersion,
        "com.vladsch.flexmark" % "flexmark-ext-gfm-strikethrough" % flexmarkVersion,
        "com.vladsch.flexmark" % "flexmark-ext-yaml-front-matter" % flexmarkVersion,
        Dependencies.`jackson-dataformat-yaml`,
        "nl.big-o" % "liqp" % "0.6.7"
      )
    }
  )

  lazy val `dotty-doc` = project.in(file("doc-tool")).asDottyDoc(NonBootstrapped)
  lazy val `dotty-doc-bootstrapped` = project.in(file("doc-tool")).asDottyDoc(Bootstrapped)

  def dottyDoc(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `dotty-doc`
    case Bootstrapped => `dotty-doc-bootstrapped`
  }

  def testOnlyFiltered(test: String, options: String) = Def.inputTaskDyn {
    val args = spaceDelimited("<arg>").parsed
    val cmd = s" $test -- $options" + {
      if (args.nonEmpty) " -Ddotty.tests.filter=" + args.mkString(" ")
      else ""
    }
    (testOnly in Test).toTask(cmd)
  }

  def findLib(attList: Seq[Attributed[File]], name: String) = attList
    .map(_.data.getAbsolutePath)
    .find(_.contains(name))
    .toList.mkString(File.pathSeparator)

  // Settings shared between dotty-compiler and dotty-compiler-bootstrapped
  lazy val commonDottyCompilerSettings = Seq(

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
            sbt.IO.copy(pairs, CopyOptions(overwrite = true, preserveLastModified = true, preserveExecutable = true))
        }

        pairs.map(_._2)
      }.taskValue,

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

      // get libraries onboard
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" % "scala-asm" % "6.0.0-scala-1", // used by the backend
        // FIXME: Not needed, but should be on the compiler CP
        ("org.scala-lang.modules" %% "scala-xml" % "1.1.0").withDottyCompat(scalaVersion.value),
        "org.scala-lang" % "scala-library" % scalacVersion % "test",
        Dependencies.`compiler-interface`,
        "org.jline" % "jline-reader" % "3.9.0",   // used by the REPL
        "org.jline" % "jline-terminal" % "3.9.0",
        "org.jline" % "jline-terminal-jna" % "3.9.0" // needed for Windows
      ),

      // For convenience, change the baseDirectory when running the compiler
      baseDirectory in (Compile, run) := baseDirectory.value / "..",
      // And when running the tests
      baseDirectory in Test := baseDirectory.value / "..",

      test in Test := {
        // Exclude VulpixMetaTests
        (testOnly in Test).toTask(" -- --exclude-categories=dotty.VulpixMetaTests").value
      },

      testOptions in Test += Tests.Argument(
        TestFrameworks.JUnit,
        "--run-listener=dotty.tools.ContextEscapeDetector",
      ),

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
        val jars = packageAll.value

        val ci_build = // propagate if this is a ci build
          sys.props.get("dotty.drone.mem") match {
            case Some(prop) => List("-Xmx" + prop)
            case _ => List()
          }

        val tuning =
          if (sys.props.isDefinedAt("Oshort"))
            // Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
            List("-XX:+TieredCompilation", "-XX:TieredStopAtLevel=1")
          else List()

        val jarOpts = List(
          "-Ddotty.tests.classes.dottyInterfaces=" + jars("dotty-interfaces"),
          "-Ddotty.tests.classes.dottyLibrary=" + jars("dotty-library"),
          "-Ddotty.tests.classes.dottyCompiler=" + jars("dotty-compiler"),
          "-Ddotty.tests.classes.compilerInterface=" + findLib(attList, "compiler-interface"),
          "-Ddotty.tests.classes.scalaLibrary=" + findLib(attList, "scala-library"),
          "-Ddotty.tests.classes.scalaAsm=" + findLib(attList, "scala-asm"),
          "-Ddotty.tests.classes.scalaXml=" + findLib(attList, "scala-xml"),
          "-Ddotty.tests.classes.jlineTerminal=" + findLib(attList, "jline-terminal"),
          "-Ddotty.tests.classes.jlineReader=" + findLib(attList, "jline-reader")
        )

        jarOpts ::: tuning ::: agentOptions ::: ci_build
      },

      testCompilation := testOnlyFiltered("dotty.tools.dotc.*CompilationTests", "--exclude-categories=dotty.SlowTests").evaluated,
      testFromTasty := testOnlyFiltered("dotty.tools.dotc.FromTastyTests", "").evaluated,

      dotr := {
        val args: List[String] = spaceDelimited("<arg>").parsed.toList
        val attList = (dependencyClasspath in Runtime).value
        val jars = packageAll.value

        val scalaLib = findLib(attList, "scala-library")
        val dottyLib = jars("dotty-library")

        def run(args: List[String]): Unit = {
          val sep = File.pathSeparator
          val fullArgs = insertClasspathInArgs(args, s".$sep$dottyLib$sep$scalaLib")
          runProcess("java" :: fullArgs, wait = true)
        }

        if (args.isEmpty) {
          println("Couldn't run `dotr` without args. Use `repl` to run the repl or add args to run the dotty application")
        } else if (scalaLib == "") {
          println("Couldn't find scala-library on classpath, please run using script in bin dir instead")
        } else if (args.contains("-with-compiler")) {
          if (!isDotty.value) {
            throw new MessageOnlyException("-with-compiler can only be used with a bootstrapped compiler")
          }
          val args1 = args.filter(_ != "-with-compiler")
          val asm = findLib(attList, "scala-asm")
          val dottyCompiler = jars("dotty-compiler")
          val dottyInterfaces = jars("dotty-interfaces")
          run(insertClasspathInArgs(args1, s"$dottyCompiler:$dottyInterfaces:$asm"))
        } else run(args)
      },

      run := dotc.evaluated,
      dotc := runCompilerMain().evaluated,
      repl := runCompilerMain(repl = true).evaluated

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
      //}.taskValue
  )

  def runCompilerMain(repl: Boolean = false) = Def.inputTaskDyn {
    val attList = (dependencyClasspath in Runtime).value
    val jars = packageAll.value
    val scalaLib = findLib(attList, "scala-library")
    val dottyLib = jars("dotty-library")
    val dottyCompiler = jars("dotty-compiler")
    val args0: List[String] = spaceDelimited("<arg>").parsed.toList
    val decompile = args0.contains("-decompile")
    val printTasty = args0.contains("-print-tasty")
    val debugFromTasty = args0.contains("-Ythrough-tasty")
    val args = args0.filter(arg => arg != "-repl" && arg != "-decompile" &&
        arg != "-with-compiler" && arg != "-Ythrough-tasty")

    val main =
      if (repl) "dotty.tools.repl.Main"
      else if (decompile || printTasty) "dotty.tools.dotc.decompiler.Main"
      else if (debugFromTasty) "dotty.tools.dotc.fromtasty.Debug"
      else "dotty.tools.dotc.Main"

    var extraClasspath = s"$scalaLib${File.pathSeparator}$dottyLib"
    if ((decompile || printTasty) && !args.contains("-classpath")) extraClasspath += s"${File.pathSeparator}."
    if (args0.contains("-with-compiler")) {
      if (!isDotty.value) {
        throw new MessageOnlyException("-with-compiler can only be used with a bootstrapped compiler")
      }
      extraClasspath += s"${File.pathSeparator}$dottyCompiler"
    }

    val fullArgs = main :: insertClasspathInArgs(args, extraClasspath)

    (runMain in Compile).toTask(fullArgs.mkString(" ", " ", ""))
  }

  def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + File.pathSeparator + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }

  lazy val nonBootstrapedDottyCompilerSettings = commonDottyCompilerSettings ++ Seq(
    // packageAll packages all and then returns a map with the abs location
    packageAll := Def.taskDyn { // Use a dynamic task to avoid loops when loading the settings
      Def.task {
        Map(
          "dotty-interfaces"    -> packageBin.in(`dotty-interfaces`, Compile).value,
          "dotty-compiler"      -> packageBin.in(Compile).value,

          // NOTE: Using dotty-library-bootstrapped here is intentional: when
          // running the compiler, we should always have the bootstrapped
          // library on the compiler classpath since the non-bootstrapped one
          // may not be binary-compatible.
          "dotty-library"       -> packageBin.in(`dotty-library-bootstrapped`, Compile).value
        ).mapValues(_.getAbsolutePath)
      }
    }.value,

    testOptions in Test += Tests.Argument(
      TestFrameworks.JUnit,
      "--exclude-categories=dotty.BootstrappedOnlyTests",
    ),
    // increase stack size for non-bootstrapped compiler, because some code
    // is only tail-recursive after bootstrap
    javaOptions in Test += "-Xss2m"
  )

  lazy val bootstrapedDottyCompilerSettings = commonDottyCompilerSettings ++ Seq(
    packageAll := {
      packageAll.in(`dotty-compiler`).value ++ Seq(
        "dotty-compiler" -> packageBin.in(Compile).value.getAbsolutePath
      )
    }
  )

  def dottyCompilerSettings(implicit mode: Mode): sbt.Def.SettingsDefinition =
    if (mode == NonBootstrapped) nonBootstrapedDottyCompilerSettings else bootstrapedDottyCompilerSettings

  lazy val `dotty-compiler` = project.in(file("compiler")).asDottyCompiler(NonBootstrapped)
  lazy val `dotty-compiler-bootstrapped` = project.in(file("compiler")).asDottyCompiler(Bootstrapped)

  def dottyCompiler(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `dotty-compiler`
    case Bootstrapped => `dotty-compiler-bootstrapped`
  }

  // Settings shared between dotty-library and dotty-library-bootstrapped
  lazy val dottyLibrarySettings = Seq(
    libraryDependencies += "org.scala-lang" % "scala-library" % scalacVersion,
    // Add version-specific source directories:
    // - files in src-non-bootstrapped will only be compiled by the reference compiler (scalac)
    // - files in src-bootstrapped will only be compiled by the current dotty compiler (non-bootstrapped and bootstrapped)
    unmanagedSourceDirectories in Compile += {
      val baseDir = baseDirectory.value
      if (isDotty.value)
        baseDir / "src-bootstrapped"
      else
        baseDir / "src-non-bootstrapped"
    }
  )

  lazy val `dotty-library` = project.in(file("library")).asDottyLibrary(NonBootstrapped)
  lazy val `dotty-library-bootstrapped`: Project = project.in(file("library")).asDottyLibrary(Bootstrapped)

  def dottyLibrary(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `dotty-library`
    case Bootstrapped => `dotty-library-bootstrapped`
  }

  // until sbt/sbt#2402 is fixed (https://github.com/sbt/sbt/issues/2402)
  lazy val cleanSbtBridge = TaskKey[Unit]("cleanSbtBridge", "delete dotty-sbt-bridge cache")

  def cleanSbtBridgeImpl(): Unit = {
    val home = System.getProperty("user.home")
    val sbtOrg = "org.scala-sbt"
    val bridgePattern = s"*dotty-sbt-bridge*$dottyVersion*"

    IO.delete((file(home) / ".sbt" / "1.0" / "zinc" / sbtOrg * bridgePattern).get)
    IO.delete((file(home) / ".sbt"  / "boot" * "scala-*" / sbtOrg / "sbt" * "*" * bridgePattern).get)
  }

  lazy val dottySbtBridgeSettings = Seq(
    cleanSbtBridge := {
      cleanSbtBridgeImpl()
    },
    compile in Compile := {
      val log = streams.value.log
      val prev = (previousCompile in Compile).value.analysis.orElse(null)
      val cur = (compile in Compile).value
      if (prev != cur) {
        log.info("Cleaning the dotty-sbt-bridge cache because it was recompiled.")
        cleanSbtBridgeImpl()
      }
      cur
    },
    description := "sbt compiler bridge for Dotty",
    resolvers += Resolver.typesafeIvyRepo("releases"), // For org.scala-sbt:api
    libraryDependencies ++= Seq(
      Dependencies.`compiler-interface` % Provided,
      (Dependencies.`zinc-api-info` % Test).withDottyCompat(scalaVersion.value)
    ),
    // The sources should be published with crossPaths := false since they
    // need to be compiled by the project using the bridge.
    crossPaths := false,

    // Don't publish any binaries for the bridge because of the above
    publishArtifact in (Compile, packageBin) := false,

    fork in Test := true,
    parallelExecution in Test := false
  )

  lazy val `dotty-sbt-bridge` = project.in(file("sbt-bridge")).asDottySbtBridge(NonBootstrapped)
  lazy val `dotty-sbt-bridge-bootstrapped` = project.in(file("sbt-bridge")).asDottySbtBridge(Bootstrapped)
    .settings(
      // Tweak -Yscala2-unpickler to allow some sbt dependencies used in tests
      /*
      scalacOptions in Test := {
        val oldOptions = (scalacOptions in Test).value
        val i = oldOptions.indexOf("-Yscala2-unpickler")
        assert(i != -1)
        val oldValue = oldOptions(i + 1)

        val attList = (dependencyClasspath in Test).value
        val sbtIo = findLib(attList, "org.scala-sbt/io")
        val zincApiInfo = findLib(attList, "zinc-apiinfo")

        oldOptions.updated(i + 1, s"$sbtIo:$zincApiInfo:$oldValue")
      }
      */
    )

  lazy val `dotty-language-server` = project.in(file("language-server")).
    dependsOn(dottyCompiler(Bootstrapped)).
    settings(commonBootstrappedSettings).
    settings(
      // Sources representing the shared configuration file used to communicate between the sbt-dotty
      // plugin and the language server
      unmanagedSourceDirectories in Compile += baseDirectory.value / "../sbt-dotty/src/dotty/tools/sbtplugin/config",

      // fork so that the shutdown hook in Main is run when we ctrl+c a run
      // (you need to have `cancelable in Global := true` in your global sbt config to ctrl+c a run)
      fork in run := true,
      fork in Test := true,
      libraryDependencies ++= Seq(
        "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.5.0",
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
    ).
    settings(
      ideTestsCompilerVersion := (version in `dotty-compiler`).value,
      ideTestsCompilerArguments := Seq(),
      ideTestsDependencyClasspath := {
        val dottyLib = (classDirectory in `dotty-library-bootstrapped` in Compile).value
        val scalaLib =
          (dependencyClasspath in `dotty-library-bootstrapped` in Compile)
            .value
            .map(_.data)
            .filter(_.getName.matches("scala-library.*\\.jar"))
            .toList
        dottyLib :: scalaLib
      },
      buildInfoKeys in Test := Seq[BuildInfoKey](
        ideTestsCompilerVersion,
        ideTestsCompilerArguments,
        ideTestsDependencyClasspath
      ),
      buildInfoPackage in Test := "dotty.tools.languageserver.util.server",
      BuildInfoPlugin.buildInfoScopedSettings(Test),
      BuildInfoPlugin.buildInfoDefaultSettings
    )

  lazy val `dotty-bench` = project.in(file("bench")).asDottyBench(NonBootstrapped)
  lazy val `dotty-bench-bootstrapped` = project.in(file("bench")).asDottyBench(Bootstrapped)

  lazy val `dotty-semanticdb` = project.in(file("semanticdb")).asDottySemanticdb(Bootstrapped)
  lazy val `dotty-semanticdb-input` = project.in(file("semanticdb/input")).settings(
    scalaVersion := "2.12.7",
    scalacOptions += "-Yrangepos",
    addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.0.0" cross CrossVersion.full)
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
    enablePlugins(SbtPlugin).
    settings(commonSettings).
    settings(
      name := sbtDottyName,
      version := sbtDottyVersion,
      // Keep in sync with inject-sbt-dotty.sbt
      libraryDependencies ++= Seq(
        Dependencies.`jackson-databind`,
        Dependencies.`compiler-interface`
      ),
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config",
      sbtTestDirectory := baseDirectory.value / "sbt-test",
      scriptedLaunchOpts ++= Seq(
        "-Dplugin.version=" + version.value,
        "-Dplugin.scalaVersion=" + dottyVersion,
        "-Dsbt.boot.directory=" + ((baseDirectory in ThisBuild).value / ".sbt-scripted").getAbsolutePath // Workaround sbt/sbt#3469
      ),
      // By default scripted tests use $HOME/.ivy2 for the ivy cache. We need to override this value for the CI.
      scriptedLaunchOpts ++= ivyPaths.value.ivyHome.map("-Dsbt.ivy.home=" + _.getAbsolutePath).toList,
      scriptedBufferLog := true,
      scripted := scripted.dependsOn(
        publishLocal in `dotty-sbt-bridge-bootstrapped`,
        publishLocal in `dotty-interfaces`,
        publishLocal in `dotty-compiler-bootstrapped`,
        publishLocal in `dotty-library-bootstrapped`,
        publishLocal in `scala-library`,
        publishLocal in `scala-reflect`,
        publishLocal in `dotty-doc-bootstrapped`,
        publishLocal in `dotty-bootstrapped` // Needed because sbt currently hardcodes the dotty artifact
      ).evaluated
    )

  lazy val `vscode-dotty` = project.in(file("vscode-dotty")).
    settings(commonSettings).
    settings(
      EclipseKeys.skipProject := true,
      version := "0.1.10-snapshot", // Keep in sync with package.json
      autoScalaLibrary := false,
      publishArtifact := false,
      includeFilter in unmanagedSources := NothingFilter | "*.ts" | "**.json",
      watchSources in Global ++= (unmanagedSources in Compile).value,
      resourceGenerators in Compile += Def.task {
        // Resources that will be copied when bootstrapping a new project
        val buildSbtFile = baseDirectory.value / "out" / "build.sbt"
        IO.write(buildSbtFile,
          s"""scalaVersion := "$publishedDottyVersion"""")
        val dottyPluginSbtFile = baseDirectory.value / "out" / "dotty-plugin.sbt"
        IO.write(dottyPluginSbtFile,
          s"""addSbtPlugin("$dottyOrganization" % "$sbtDottyName" % "$publishedSbtDottyVersion")""")
        Seq(buildSbtFile, dottyPluginSbtFile)
      },
      compile in Compile := Def.task {
        val workingDir = baseDirectory.value
        val coursier = workingDir / "out" / "coursier"
        val packageJson = workingDir / "package.json"
        if (!coursier.exists || packageJson.lastModified > coursier.lastModified)
          runProcess(Seq("npm", "install"), wait = true, directory = Some(workingDir))
        val tsc = workingDir / "node_modules" / ".bin" / "tsc"
        runProcess(Seq(tsc.getAbsolutePath, "--pretty", "--project", workingDir.getAbsolutePath), wait = true)

        // vscode-dotty depends on scala-lang.scala for syntax highlighting,
        // this is not automatically installed when starting the extension in development mode
        // (--extensionDevelopmentPath=...)
        installCodeExtension(codeCommand.value, "scala-lang.scala")

        sbt.internal.inc.Analysis.Empty
      }.dependsOn(managedResources in Compile).value,
      sbt.Keys.`package`:= {
        runProcess(Seq("vsce", "package"), wait = true, directory = Some(baseDirectory.value))

        baseDirectory.value / s"dotty-${version.value}.vsix"
      },
      unpublish := {
        runProcess(Seq("vsce", "unpublish"), wait = true, directory = Some(baseDirectory.value))
      },
      publish := {
        runProcess(Seq("vsce", "publish"), wait = true, directory = Some(baseDirectory.value))
      },
      run := Def.inputTask {
        val inputArgs = spaceDelimited("<arg>").parsed
        val codeArgs = if (inputArgs.isEmpty) List((baseDirectory.value / "..").getAbsolutePath) else inputArgs
        val extensionPath = baseDirectory.value.getAbsolutePath
        val processArgs = List(s"--extensionDevelopmentPath=$extensionPath") ++ codeArgs

        runProcess(codeCommand.value ++ processArgs, wait = true)
      }.dependsOn(compile in Compile).evaluated
    )


  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    isSnapshot := version.value.contains("SNAPSHOT"),
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
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
    developers := List(
      Developer(
        id = "odersky",
        name = "Martin Odersky",
        email = "martin.odersky@epfl.ch",
        url = url("https://github.com/odersky")
      ),
      Developer(
        id = "DarkDimius",
        name = "Dmitry Petrashko",
        email = "me@d-d.me",
        url = url("https://d-d.me")
      ),
      Developer(
        id = "smarter",
        name = "Guillaume Martres",
        email = "smarter@ubuntu.com",
        url = url("http://guillaume.martres.me")
      ),
      Developer(
        id = "felixmulder",
        name = "Felix Mulder",
        email = "felix.mulder@gmail.com",
        url = url("http://felixmulder.com")
      ),
      Developer(
        id = "liufengyun",
        name = "Liu Fengyun",
        email = "liufengyun@chaos-lab.com",
        url = url("http://chaos-lab.com")
      ),
      Developer(
        id = "nicolasstucki",
        name = "Nicolas Stucki",
        email = "nicolas.stucki@gmail.com",
        url = url("https://github.com/nicolasstucki")
      ),
      Developer(
        id = "OlivierBlanvillain",
        name = "Olivier Blanvillain",
        email = "olivier.blanvillain@gmail.com",
        url = url("https://github.com/OlivierBlanvillain")
      ),
      Developer(
        id = "biboudis",
        name = "Aggelos Biboudis",
        email = "aggelos.biboudis@epfl.ch",
        url = url("http://biboudis.github.io")
      ),
      Developer(
        id = "allanrenucci",
        name = "Allan Renucci",
        email = "allan.renucci@gmail.com",
        url = url("https://github.com/allanrenucci")
      ),
      Developer(
        id = "Duhemm",
        name = "Martin Duhem",
        email = "martin.duhem@gmail.com",
        url = url("https://github.com/Duhemm")
      )
    )
  )

  // Compile with dotty
  lazy val compileWithDottySettings = {
    inConfig(Compile)(inTask(compile)(Defaults.runnerTask) ++ Seq(
      // Compile with dotty
      fork in compile := true,

      compile := {
        val inputs = (compileInputs in compile).value
        val inputOptions = inputs.options()
        import inputOptions._

        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory

        // Discover classpaths

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(File.pathSeparator)

        val compilerCp = Attributed.data((fullClasspath in (`dotty-compiler`, Compile)).value)
        val cpStr = cpToString(classpath ++ compilerCp)

        // List all my dependencies (recompile if any of these changes)

        val allMyDependencies = classpath filterNot (_ == classesDirectory) flatMap { cpFile =>
          if (cpFile.isDirectory) (cpFile ** "*.class").get
          else Seq(cpFile)
        }

        // Compile

        val run = (runner in compile).value
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
            run.run("dotty.tools.dotc.Main", compilerCp,
                "-classpath" :: cpStr ::
                "-d" :: classesDirectory.getAbsolutePath ::
                scalacOptions ++:
                sourcesArgs,
                patchedLogger)
          }

          // Work around the Windows limitation on command line length.
          val isWindows =
            System.getProperty("os.name").toLowerCase().indexOf("win") >= 0
          if ((fork in compile).value && isWindows &&
              (sourcesArgs.map(_.length).sum > 1536)) {
            IO.withTemporaryFile("sourcesargs", ".txt") { sourceListFile =>
              IO.writeLines(sourceListFile, sourcesArgs)
              doCompile(List("@"+sourceListFile.getAbsolutePath))
            }
          } else {
            doCompile(sourcesArgs)
          }

          // Output is all files in classesDirectory
          (classesDirectory ** AllPassFilter).get.toSet
        }

        cachedCompile((sources ++ allMyDependencies).toSet)

        // We do not have dependency analysis when compiling externally
        sbt.internal.inc.Analysis.Empty
      }
    ))
  }

  lazy val commonDistSettings = Seq(
    packMain := Map(),
    publishArtifact := false,
    packGenerateMakefile := false,
    packExpandedClasspath := true,
    packArchiveName := "dotty-" + dottyVersion
  )

  lazy val dist = project.asDist(NonBootstrapped)
    .settings(
      packResourceDir += (baseDirectory.value / "bin" -> "bin"),
    )
  lazy val `dist-bootstrapped` = project.asDist(Bootstrapped)
    .settings(
      packResourceDir += ((baseDirectory in dist).value / "bin" -> "bin"),
    )

  // /** A sandbox to play with the Scala.js back-end of dotty.
  //  *
  //  *  This sandbox is compiled with dotty with support for Scala.js. It can be
  //  *  used like any regular Scala.js project. In particular, `fastOptJS` will
  //  *  produce a .js file, and `run` will run the JavaScript code with a JS VM.
  //  *
  //  *  Simply running `dotty/run -scalajs` without this sandbox is not very
  //  *  useful, as that would not provide the linker and JS runners.
  //  */
  // lazy val sjsSandbox = project.in(file("sandbox/scalajs")).
  //   enablePlugins(ScalaJSPlugin).
  //   settings(commonNonBootstrappedSettings).
  //   settings(
  //     /* Remove the Scala.js compiler plugin for scalac, and enable the
  //      * Scala.js back-end of dotty instead.
  //      */
  //     libraryDependencies ~= { deps =>
  //       deps.filterNot(_.name.startsWith("scalajs-compiler"))
  //     },
  //     scalacOptions += "-scalajs",

  //     // The main class cannot be found automatically due to the empty inc.Analysis
  //     mainClass in Compile := Some("hello.world"),

  //     // While developing the Scala.js back-end, it is very useful to see the trees dotc gives us
  //     scalacOptions += "-Xprint:collectSuperCalls",

  //     /* Debug-friendly Scala.js optimizer options.
  //      * In particular, typecheck the Scala.js IR found on the classpath.
  //      */
  //     scalaJSOptimizerOptions ~= {
  //       _.withCheckScalaJSIR(true).withParallel(false)
  //     }
  //   ).
  //   settings(compileWithDottySettings).
  //   settings(inConfig(Compile)(Seq(
  //     /* Make sure jsDependencyManifest runs after compile, otherwise compile
  //      * might remove the entire directory afterwards.
  //      */
  //     jsDependencyManifest := jsDependencyManifest.dependsOn(compile).value
  //   )))

  implicit class ProjectDefinitions(val project: Project) extends AnyVal {

    // FIXME: we do not aggregate `bin` because its tests delete jars, thus breaking other tests
    def asDottyRoot(implicit mode: Mode): Project = project.withCommonSettings.
      aggregate(`dotty-interfaces`, dottyLibrary, dottyCompiler, dottyDoc, dottySbtBridgeReference).
      bootstrappedAggregate(`scala-library`, `scala-compiler`, `scala-reflect`, scalap, `dotty-language-server`).
      dependsOn(dottyCompiler).
      dependsOn(dottyLibrary).
      nonBootstrappedSettings(
        addCommandAlias("run", "dotty-compiler/run")
      )

    def asDottyCompiler(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(`dotty-interfaces`).
      dependsOn(dottyLibrary).
      settings(dottyCompilerSettings)

    def asDottyLibrary(implicit mode: Mode): Project = project.withCommonSettings.
      settings(dottyLibrarySettings).
      bootstrappedSettings(
        // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called.
        scalacOptions in Compile ++= Seq("-sourcepath", (scalaSource in Compile).value.getAbsolutePath)
      )

    def asDottyDoc(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler, dottyCompiler % "test->test").
      settings(dottyDocSettings)

    def asDottySbtBridge(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler % Provided).
      dependsOn(dottyDoc % Provided).
      settings(dottySbtBridgeSettings)

    def asDottyBench(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler).
      settings(commonBenchmarkSettings).
      enablePlugins(JmhPlugin)

    def asDottySemanticdb(implicit mode: Mode): Project = project.withCommonSettings.
      aggregate(`dotty-semanticdb-input`).
      dependsOn(dottyCompiler).
      settings(semanticdbSettings)

    def asDist(implicit mode: Mode): Project = project.
      enablePlugins(PackPlugin).
      withCommonSettings.
      dependsOn(`dotty-interfaces`, dottyCompiler, dottyLibrary, dottyDoc).
      settings(commonDistSettings).
      bootstrappedSettings(
        target := baseDirectory.value / "target" // override setting in commonBootstrappedSettings
      )

    def withCommonSettings(implicit mode: Mode): Project = project.settings(mode match {
      case NonBootstrapped => commonNonBootstrappedSettings
      case Bootstrapped => commonBootstrappedSettings
    })
  }
}
