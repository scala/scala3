import java.io.File
import java.nio.file._

import Modes._
import com.jsuereth.sbtpgp.PgpKeys
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
import xerial.sbt.Sonatype.autoImport._

import dotty.tools.sbtplugin.DottyPlugin.autoImport._
import dotty.tools.sbtplugin.DottyPlugin.makeScalaInstance
import dotty.tools.sbtplugin.DottyIDEPlugin.{ installCodeExtension, prepareCommand, runProcess }
import dotty.tools.sbtplugin.DottyIDEPlugin.autoImport._

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._

import scala.util.Properties.isJavaAtLeast

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object MyScalaJSPlugin extends AutoPlugin {
  import Build._

  override def requires: Plugins = ScalaJSPlugin

  override def projectSettings: Seq[Setting[_]] = Def.settings(
    commonBootstrappedSettings,

    // Replace the JVM JUnit dependency by the Scala.js one
    libraryDependencies ~= {
      _.filter(!_.name.startsWith("junit-interface"))
    },
    libraryDependencies +=
      ("org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion  % "test").withDottyCompat(scalaVersion.value),

    // Typecheck the Scala.js IR found on the classpath
    scalaJSLinkerConfig ~= (_.withCheckIR(true)),

    // Exclude all these projects from `configureIDE/launchIDE` since they
    // take time to compile, print a bunch of warnings, and are rarely edited.
    excludeFromIDE := true
  )
}

object Build {
  val referenceVersion = "3.0.0-RC1-bin-20210122-6947b0f-NIGHTLY"

  val baseVersion = "3.0.0-RC1"
  val baseSbtDottyVersion = "0.5.3"

  // Versions used by the vscode extension to create a new project
  // This should be the latest published releases.
  // TODO: Have the vscode extension fetch these numbers from the Internet
  // instead of hardcoding them ?
  val publishedDottyVersion = referenceVersion
  val publishedSbtDottyVersion = "0.5.2"

  /** scala-library version required to compile Dotty.
   *
   *  Both the non-bootstrapped and bootstrapped version should match, unless
   *  we're in the process of upgrading to a new major version of
   *  scala-library.
   */
  def stdlibVersion(implicit mode: Mode): String = mode match {
    case NonBootstrapped => "2.13.4"
    case Bootstrapped => "2.13.4"
  }

  val dottyOrganization = "org.scala-lang"
  val dottyGithubUrl = "https://github.com/lampepfl/dotty"
  val dottyGithubRawUserContentUrl = "https://raw.githubusercontent.com/lampepfl/dotty"


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
  val packageAll = taskKey[Map[String, String]]("Package everything needed to run tests")

  // Run tests with filter through vulpix test suite
  val testCompilation = inputKey[Unit]("runs integration test with the supplied filter")

  // Spawns a repl with the correct classpath
  val repl = inputKey[Unit]("run the REPL with correct classpath")

  // Used to compile files similar to ./bin/scalac script
  val scalac = inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

  // Used to run binaries similar to ./bin/scala script
  val scala = inputKey[Unit]("run compiled binary using the correct classpath, or the user supplied classpath")

  // Compiles the documentation and static site
  val genDocs = inputKey[Unit]("run dottydoc to generate static documentation site")

  // Shorthand for compiling a docs site
  val dottydoc = inputKey[Unit]("run dottydoc")

  // Only available in vscode-dotty
  val unpublish = taskKey[Unit]("Unpublish a package")

  // Settings used to configure the test language server
  val ideTestsCompilerVersion = taskKey[String]("Compiler version to use in IDE tests")
  val ideTestsCompilerArguments = taskKey[Seq[String]]("Compiler arguments to use in IDE tests")
  val ideTestsDependencyClasspath = taskKey[Seq[File]]("Dependency classpath to use in IDE tests")

  val fetchScalaJSSource = taskKey[File]("Fetch the sources of Scala.js")

  lazy val SourceDeps = config("sourcedeps")

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
      "-language:existentials,higherKinds,implicitConversions,postfixOps"
    ),

    javacOptions in (Compile, compile) ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    // Override `runCode` from sbt-dotty to use the language-server and
    // vscode extension from the source repository of dotty instead of a
    // published version.
    runCode := (run in `scala3-language-server`).toTask("").value,

    // Avoid various sbt craziness involving classloaders and parallelism
    fork in run := true,
    fork in Test := true,
    parallelExecution in Test := false,

    outputStrategy := Some(StdoutOutput),

    // enable verbose exception messages for JUnit
    testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
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

      // Copy default configuration from .vscode-template/ unless configuration files already exist in .vscode/
      sbt.IO.copyDirectory(new File(".vscode-template/"), new File(".vscode/"), overwrite = false)

      state
    },

    // I find supershell more distracting than helpful
    useSuperShell := false,

    // Credentials to release to Sonatype
    credentials ++= (
      for {
        username <- sys.env.get("SONATYPE_USER")
        password <- sys.env.get("SONATYPE_PW")
      } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
    ).toList,
    PgpKeys.pgpPassphrase := sys.env.get("PGP_PW").map(_.toCharArray()),
    PgpKeys.useGpgPinentry := true,

    javaOptions ++= {
      val ciOptions = // propagate if this is a CI build
        sys.props.get("dotty.drone.mem") match {
          case Some(prop) => List("-Xmx" + prop)
          case _ => List()
        }
      // Do not cut off the bottom of large stack traces (default is 1024)
      "-XX:MaxJavaStackTraceDepth=1000000" :: agentOptions ::: ciOptions
    },

    excludeLintKeys ++= Set(
      // We set these settings in `commonSettings`, if a project
      // uses `commonSettings` but overrides `unmanagedSourceDirectories`,
      // sbt will complain if we don't exclude them here.
      Keys.scalaSource, Keys.javaSource
    ),
  )

  lazy val disableDocSetting =
    // Disable scaladoc generation, it's way too slow and we'll replace it
    // by dottydoc anyway. We still publish an empty -javadoc.jar to make
    // sonatype happy.
      sources in (Compile, doc) := Seq()

  lazy val commonSettings = publishSettings ++ Seq(
    scalaSource       in Compile    := baseDirectory.value / "src",
    scalaSource       in Test       := baseDirectory.value / "test",
    javaSource        in Compile    := baseDirectory.value / "src",
    javaSource        in Test       := baseDirectory.value / "test",
    resourceDirectory in Compile    := baseDirectory.value / "resources",
    resourceDirectory in Test       := baseDirectory.value / "test-resources",

    // Prevent sbt from rewriting our dependencies
    scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(false))),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,

    // If someone puts a source file at the root (e.g., for manual testing),
    // don't pick it up as part of any project.
    sourcesInBase := false,
  )

  // Settings used for projects compiled only with Java
  lazy val commonJavaSettings = commonSettings ++ Seq(
    version := dottyVersion,
    scalaVersion := referenceVersion,
    // Do not append Scala versions to the generated artifacts
    crossPaths := false,
    // Do not depend on the Scala library
    autoScalaLibrary := false,
    excludeFromIDE := true,
    disableDocSetting
  )

  // Settings used when compiling dotty (both non-bootstrapped and bootstrapped)
  lazy val commonDottySettings = commonSettings ++ Seq(
    // Manually set the standard library to use
    autoScalaLibrary := false
  )

  lazy val commonScala2Settings = commonSettings ++ Seq(
    scalaVersion := stdlibVersion(Bootstrapped),
    moduleName ~= { _.stripSuffix("-scala2") },
    version := dottyVersion,
    target := baseDirectory.value / ".." / "out" / "scala-2" / name.value,

    disableDocSetting
  )

  // Settings used when compiling dotty with the reference compiler
  lazy val commonNonBootstrappedSettings = commonDottySettings ++ Seq(
    unmanagedSourceDirectories in Compile += baseDirectory.value / "src-non-bootstrapped",

    version := dottyNonBootstrappedVersion,
    scalaVersion := referenceVersion,
    excludeFromIDE := true,

    disableDocSetting
  )

  // Settings used when compiling dotty with a non-bootstrapped dotty
  lazy val commonBootstrappedSettings = commonDottySettings ++ Seq(
    unmanagedSourceDirectories in Compile += baseDirectory.value / "src-bootstrapped",

    version := dottyVersion,
    scalaVersion := dottyNonBootstrappedVersion,

    scalaCompilerBridgeBinaryJar := {
      Some((packageBin in (`scala3-sbt-bridge`, Compile)).value)
    },

    // Use the same name as the non-bootstrapped projects for the artifacts.
    // Remove the `js` suffix because JS artifacts are published using their special crossVersion.
    // The order of the two `stripSuffix`es is important, so that
    // scala3-library-bootstrappedjs becomes scala3-library.
    moduleName ~= { _.stripSuffix("js").stripSuffix("-bootstrapped") },

    // Enforce that the only Scala 2 classfiles we unpickle come from scala-library
    /*
    scalacOptions ++= {
      val cp = (dependencyClasspath in `scala3-library` in Compile).value
      val scalaLib = findArtifactPath(cp, "scala-library")
      Seq("-Yscala2-unpickler", scalaLib)
    },
    */

    // sbt gets very unhappy if two projects use the same target
    target := baseDirectory.value / ".." / "out" / "bootstrap" / name.value,

    // Compile using the non-bootstrapped and non-published dotty
    managedScalaInstance := false,
    scalaInstance := {
      val externalNonBootstrappedDeps = externalDependencyClasspath.in(`scala3-doc`, Compile).value
      val scalaLibrary = findArtifact(externalNonBootstrappedDeps, "scala-library")

      // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
      // just directories containing classfiles because sbt maintains a cache of
      // compiler instances. This cache is invalidated based on timestamps
      // however this is only implemented on jars, directories are never
      // invalidated.
      val tastyCore = packageBin.in(`tasty-core`, Compile).value
      val dottyLibrary = packageBin.in(`scala3-library`, Compile).value
      val dottyInterfaces = packageBin.in(`scala3-interfaces`, Compile).value
      val dottyCompiler = packageBin.in(`scala3-compiler`, Compile).value
      val dottyDoc = packageBin.in(`scala3-doc`, Compile).value

      val allJars = Seq(tastyCore, dottyLibrary, dottyInterfaces, dottyCompiler, dottyDoc) ++ externalNonBootstrappedDeps.map(_.data)

      makeScalaInstance(
        state.value,
        scalaVersion.value,
        scalaLibrary,
        dottyLibrary,
        dottyCompiler,
        allJars,
        appConfiguration.value
      )
    },
    // sbt-dotty defines `scalaInstance in doc` so we need to override it manually
    scalaInstance in doc := scalaInstance.value,

    disableDocSetting,
  )

  lazy val commonBenchmarkSettings = Seq(
    mainClass in (Jmh, run) := Some("dotty.tools.benchmarks.Bench"), // custom main for jmh:run
    javaOptions += "-DBENCH_COMPILER_CLASS_PATH=" + Attributed.data((fullClasspath in (`scala3-bootstrapped`, Compile)).value).mkString("", File.pathSeparator, ""),
    javaOptions += "-DBENCH_CLASS_PATH=" + Attributed.data((fullClasspath in (`scala3-library-bootstrapped`, Compile)).value).mkString("", File.pathSeparator, "")
  )

  /** Projects -------------------------------------------------------------- */

  val dottyCompilerBootstrappedRef = LocalProject("scala3-compiler-bootstrapped")

  /** External dependencies we may want to put on the compiler classpath. */
  def externalCompilerClasspathTask: Def.Initialize[Task[Def.Classpath]] =
    // Even if we're running the non-bootstrapped compiler, we want the
    // dependencies of the bootstrapped compiler since we want to put them on
    // the compiler classpath, not the JVM classpath.
    externalDependencyClasspath.in(dottyCompilerBootstrappedRef, Runtime)

  // The root project:
  // - aggregates other projects so that "compile", "test", etc are run on all projects at once.
  // - publishes its own empty artifact "dotty" that depends on "scala3-library" and "scala3-compiler",
  //   this is only necessary for compatibility with sbt which currently hardcodes the "dotty" artifact name
  lazy val scala3 = project.in(file(".")).asDottyRoot(NonBootstrapped)
  lazy val `scala3-bootstrapped` = project.asDottyRoot(Bootstrapped)

  lazy val `scala3-interfaces` = project.in(file("interfaces")).
    settings(commonJavaSettings)

  private lazy val dottydocClasspath = Def.task {
    val jars = (packageAll in `scala3-compiler`).value
    val dottyLib = jars("scala3-library")
    val otherDeps = (dependencyClasspath in Compile).value.map(_.data).mkString(File.pathSeparator)
    val externalDeps = externalCompilerClasspathTask.value
    dottyLib + File.pathSeparator + findArtifactPath(externalDeps, "scala-library")
  }

  lazy val commonDocSettings = Seq(
    baseDirectory in (Compile, run) := baseDirectory.value / "..",
    baseDirectory in Test := baseDirectory.value / "..",
    libraryDependencies ++= {
      val flexmarkVersion = "0.42.12"
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

  def dottyDocSettings(implicit mode: Mode) = Seq(
    connectInput in run := true,

    javaOptions ++= (javaOptions in `scala3-compiler`).value,

    javaOptions += "-Xss3m",

    genDocs := Def.inputTaskDyn {
      val dottydocExtraArgs = spaceDelimited("<arg>").parsed

      // Make majorVersion available at dotty.epfl.ch/versions/latest-nightly-base
      // Used by sbt-dotty to resolve the latest nightly
      val majorVersion = (scalaBinaryVersion in LocalProject("scala3-library-bootstrapped")).value
      IO.write(file("./docs/_site/versions/latest-nightly-base"), majorVersion)

      // This file is used by GitHub Pages when the page is available in a custom domain
      IO.write(file("./docs/_site/CNAME"), "dotty.epfl.ch")

      val sources = unmanagedSources.in(dottyLibrary, Compile).value
      val args = Seq(
        "-siteroot", "docs",
        "-project", "Dotty",
        "-project-version", dottyVersion,
        "-project-url", dottyGithubUrl,
        "-project-logo", "scala3-logo.svg",
        "-classpath", dottydocClasspath.value,
        "-Yerased-terms"
      ) ++ dottydocExtraArgs
      (runMain in Compile).toTask(
        s""" dotty.tools.dottydoc.Main ${args.mkString(" ")} ${sources.mkString(" ")}"""
      )
    }.evaluated,

    dottydoc := Def.inputTaskDyn {
      val args = spaceDelimited("<arg>").parsed
      val cp = dottydocClasspath.value

      (runMain in Compile).toTask(s" dotty.tools.dottydoc.Main -classpath $cp " + args.mkString(" "))
    }.evaluated,
  )

  lazy val `scala3-doc` = project.in(file("doc-tool")).asDottyDoc(NonBootstrapped)
  lazy val `scala3-doc-bootstrapped` = project.in(file("doc-tool")).asDottyDoc(Bootstrapped)

  def dottyDoc(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `scala3-doc`
    case Bootstrapped => `scala3-doc-bootstrapped`
  }

  /** Find an artifact with the given `name` in `classpath` */
  def findArtifact(classpath: Def.Classpath, name: String): File = classpath
    .find(_.get(artifact.key).exists(_.name == name))
    .getOrElse(throw new MessageOnlyException(s"Artifact for $name not found in $classpath"))
    .data

  /** Like `findArtifact` but returns the absolute path of the entry as a string */
  def findArtifactPath(classpath: Def.Classpath, name: String): String =
    findArtifact(classpath, name).getAbsolutePath

  // Settings shared between scala3-compiler and scala3-compiler-bootstrapped
  lazy val commonDottyCompilerSettings = Seq(
      // set system in/out for repl
      connectInput in run := true,

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
        "org.scala-lang.modules" % "scala-asm" % "9.0.0-scala-1", // used by the backend
        Dependencies.oldCompilerInterface, // we stick to the old version to avoid deprecation warnings
        "org.jline" % "jline-reader" % "3.15.0",   // used by the REPL
        "org.jline" % "jline-terminal" % "3.15.0",
        "org.jline" % "jline-terminal-jna" % "3.15.0" // needed for Windows
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

      // Add git-hash used to package the distribution to the manifest to know it in runtime and report it in REPL
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)),

      javaOptions ++= {
        val managedSrcDir = {
          // Populate the directory
          (managedSources in Compile).value

          (sourceManaged in Compile).value
        }
        val externalDeps = externalCompilerClasspathTask.value
        val jars = packageAll.value

        Seq(
          "-Ddotty.tests.dottyCompilerManagedSources=" + managedSrcDir,
          "-Ddotty.tests.classes.dottyInterfaces=" + jars("scala3-interfaces"),
          "-Ddotty.tests.classes.dottyLibrary=" + jars("scala3-library"),
          "-Ddotty.tests.classes.dottyCompiler=" + jars("scala3-compiler"),
          "-Ddotty.tests.classes.tastyCore=" + jars("tasty-core"),
          "-Ddotty.tests.classes.compilerInterface=" + findArtifactPath(externalDeps, "compiler-interface"),
          "-Ddotty.tests.classes.scalaLibrary=" + findArtifactPath(externalDeps, "scala-library"),
          "-Ddotty.tests.classes.scalaAsm=" + findArtifactPath(externalDeps, "scala-asm"),
          "-Ddotty.tests.classes.jlineTerminal=" + findArtifactPath(externalDeps, "jline-terminal"),
          "-Ddotty.tests.classes.jlineReader=" + findArtifactPath(externalDeps, "jline-reader"),
        )
      },

      javaOptions += (
        s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}"
      ),

      testCompilation := Def.inputTaskDyn {
        val args = spaceDelimited("<arg>").parsed
        if (args.contains("--help")) {
          println(
            s"""
               |usage: testCompilation [--help] [--from-tasty] [--update-checkfiles] [<filter>]
               |
               |By default runs tests in dotty.tools.dotc.*CompilationTests excluding tests tagged with dotty.SlowTests.
               |
               |  --help                show this message
               |  --from-tasty          runs tests in dotty.tools.dotc.FromTastyTests
               |  --update-checkfiles   override the checkfiles that did not match with the current output
               |  <filter>              substring of the path of the tests file
               |
             """.stripMargin
          )
          (testOnly in Test).toTask(" not.a.test")
        }
        else {
          val updateCheckfile = args.contains("--update-checkfiles")
          val fromTasty = args.contains("--from-tasty")
          val args1 = if (updateCheckfile | fromTasty) args.filter(x => x != "--update-checkfiles" && x != "--from-tasty") else args
          val test = if (fromTasty) "dotty.tools.dotc.FromTastyTests" else "dotty.tools.dotc.*CompilationTests"
          val cmd = s" $test -- --exclude-categories=dotty.SlowTests" +
            (if (updateCheckfile) " -Ddotty.tests.updateCheckfiles=TRUE" else "") +
            (if (args1.nonEmpty) " -Ddotty.tests.filter=" + args1.mkString(" ") else "")
          (testOnly in Test).toTask(cmd)
        }
      }.evaluated,

      scala := {
        val args: List[String] = spaceDelimited("<arg>").parsed.toList
        val externalDeps = externalCompilerClasspathTask.value
        val jars = packageAll.value

        val scalaLib = findArtifactPath(externalDeps, "scala-library")
        val dottyLib = jars("scala3-library")

        def run(args: List[String]): Unit = {
          val fullArgs = insertClasspathInArgs(args, List(".", dottyLib, scalaLib).mkString(File.pathSeparator))
          runProcess("java" :: fullArgs, wait = true)
        }

        if (args.isEmpty) {
          println("Couldn't run `scala` without args. Use `repl` to run the repl or add args to run the dotty application")
        } else if (scalaLib == "") {
          println("Couldn't find scala-library on classpath, please run using script in bin dir instead")
        } else if (args.contains("-with-compiler")) {
          val args1 = args.filter(_ != "-with-compiler")
          val asm = findArtifactPath(externalDeps, "scala-asm")
          val dottyCompiler = jars("scala3-compiler")
          val dottyStaging = jars("scala3-staging")
          val dottyTastyInspector = jars("scala3-tasty-inspector")
          val dottyInterfaces = jars("scala3-interfaces")
          val tastyCore = jars("tasty-core")
          run(insertClasspathInArgs(args1, List(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore).mkString(File.pathSeparator)))
        } else run(args)
      },

      run := scalac.evaluated,
      scalac := runCompilerMain().evaluated,
      repl := runCompilerMain(repl = true).evaluated,

      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps").withDottyCompat(scalaVersion.value),
      sourceGenerators in Compile += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (sourceManaged in Compile).value / "scalajs-ir-src"

        val report = updateClassifiers.value
        val scalaJSIRSourcesJar = report.select(
            configuration = configurationFilter("sourcedeps"),
            module = (_: ModuleID).name.startsWith("scalajs-ir_"),
            artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
          sys.error(s"Could not fetch scalajs-ir sources")
        }

        FileFunction.cached(cacheDir / s"fetchScalaJSIRSource",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info(s"Unpacking scalajs-ir sources to $trgDir...")
          if (trgDir.exists)
            IO.delete(trgDir)
          IO.createDirectory(trgDir)
          IO.unzip(scalaJSIRSourcesJar, trgDir)

          (trgDir ** "*.scala").get.toSet
        } (Set(scalaJSIRSourcesJar)).toSeq
      }.taskValue,
  )

  def runCompilerMain(repl: Boolean = false) = Def.inputTaskDyn {
    val log = streams.value.log
    val externalDeps = externalCompilerClasspathTask.value
    val jars = packageAll.value
    val scalaLib = findArtifactPath(externalDeps, "scala-library")
    val dottyLib = jars("scala3-library")
    val dottyCompiler = jars("scala3-compiler")
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

    var extraClasspath = Seq(scalaLib, dottyLib)

    if ((decompile || printTasty) && !args.contains("-classpath"))
      extraClasspath ++= Seq(".")

    if (args0.contains("-with-compiler")) {
      if (scalaVersion.value == referenceVersion) {
        log.error("-with-compiler should only be used with a bootstrapped compiler")
      }
      val dottyInterfaces = jars("scala3-interfaces")
      val dottyStaging = jars("scala3-staging")
      val dottyTastyInspector = jars("scala3-tasty-inspector")
      val tastyCore = jars("tasty-core")
      val asm = findArtifactPath(externalDeps, "scala-asm")
      extraClasspath ++= Seq(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore)
    }

    val fullArgs = main :: insertClasspathInArgs(args, extraClasspath.mkString(File.pathSeparator))

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
          "scala3-interfaces"    -> packageBin.in(`scala3-interfaces`, Compile).value,
          "scala3-compiler"      -> packageBin.in(Compile).value,
          "tasty-core"          -> packageBin.in(`tasty-core`, Compile).value,

          // NOTE: Using scala3-library-bootstrapped here is intentional: when
          // running the compiler, we should always have the bootstrapped
          // library on the compiler classpath since the non-bootstrapped one
          // may not be binary-compatible.
          "scala3-library"       -> packageBin.in(`scala3-library-bootstrapped`, Compile).value
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
    javaOptions ++= {
      val jars = packageAll.value
      Seq(
        "-Ddotty.tests.classes.dottyStaging=" + jars("scala3-staging"),
        "-Ddotty.tests.classes.dottyTastyInspector=" + jars("scala3-tasty-inspector"),
      )
    },
    packageAll := {
      packageAll.in(`scala3-compiler`).value ++ Seq(
        "scala3-compiler" -> packageBin.in(Compile).value.getAbsolutePath,
        "scala3-staging"  -> packageBin.in(LocalProject("scala3-staging"), Compile).value.getAbsolutePath,
        "scala3-tasty-inspector"  -> packageBin.in(LocalProject("scala3-tasty-inspector"), Compile).value.getAbsolutePath,
        "tasty-core"     -> packageBin.in(LocalProject("tasty-core-bootstrapped"), Compile).value.getAbsolutePath,
      )
    }
  )

  def dottyCompilerSettings(implicit mode: Mode): sbt.Def.SettingsDefinition =
    if (mode == NonBootstrapped) nonBootstrapedDottyCompilerSettings else bootstrapedDottyCompilerSettings

  lazy val `scala3-compiler` = project.in(file("compiler")).asDottyCompiler(NonBootstrapped)
  lazy val `scala3-compiler-bootstrapped` = project.in(file("compiler")).asDottyCompiler(Bootstrapped)

  def dottyCompiler(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `scala3-compiler`
    case Bootstrapped => `scala3-compiler-bootstrapped`
  }

  // Settings shared between scala3-library, scala3-library-bootstrapped and scala3-library-bootstrappedJS
  lazy val dottyLibrarySettings = Seq(
    scalacOptions in Compile ++= Seq(
      // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called
      "-sourcepath", (sourceDirectories in Compile).value.map(_.getAbsolutePath).distinct.mkString(File.pathSeparator),
     // support declaration of scala.compiletime.erasedValue
      "-Yerased-terms"
    ),
  )

  lazy val `scala3-library` = project.in(file("library")).asDottyLibrary(NonBootstrapped)
  lazy val `scala3-library-bootstrapped`: Project = project.in(file("library")).asDottyLibrary(Bootstrapped)

  def dottyLibrary(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `scala3-library`
    case Bootstrapped => `scala3-library-bootstrapped`
  }

  /** The dotty standard library compiled with the Scala.js back-end, to produce
   *  the corresponding .sjsir files.
   *
   *  This artifact must be on the classpath on every "Dotty.js" project.
   *
   *  Currently, only a very small fraction of the dotty library is actually
   *  included in this project, and hence available to Dotty.js projects. More
   *  will be added in the future as things are confirmed to be supported.
   */
  lazy val `scala3-library-bootstrappedJS`: Project = project.in(file("library-js")).
    asDottyLibrary(Bootstrapped).
    enablePlugins(MyScalaJSPlugin).
    settings(
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-library" % scalaJSVersion).withDottyCompat(scalaVersion.value),
      unmanagedSourceDirectories in Compile :=
        (unmanagedSourceDirectories in (`scala3-library-bootstrapped`, Compile)).value,

      // Configure the source maps to point to GitHub for releases
      scalacOptions ++= {
        if (isRelease) {
          val baseURI = (baseDirectory in LocalRootProject).value.toURI
          val dottyVersion = version.value
          Seq(s"-scalajs-mapSourceURI:$baseURI->$dottyGithubRawUserContentUrl/v$dottyVersion/")
        } else {
          Nil
        }
      },

      // Make sure `scala3-bootstrapped/test` doesn't fail on this project for no reason
      test in Test := {},
      testOnly in Test := {},
    )

  lazy val tastyCoreSettings = Seq(
    scalacOptions += "-source:3.0-migration"
  )

  lazy val `tasty-core` = project.in(file("tasty")).asTastyCore(NonBootstrapped)
  lazy val `tasty-core-bootstrapped`: Project = project.in(file("tasty")).asTastyCore(Bootstrapped)
  lazy val `tasty-core-scala2`: Project = project.in(file("tasty")).asTastyCoreScala2

  def tastyCore(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `tasty-core`
    case Bootstrapped => `tasty-core-bootstrapped`
  }

  lazy val `scala3-staging` = project.in(file("staging")).
    withCommonSettings(Bootstrapped).
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-staging (see sbt-dotty/sbt-test/sbt-dotty/quoted-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(commonBootstrappedSettings).
    settings(
      javaOptions := (javaOptions in `scala3-compiler-bootstrapped`).value
    )

  lazy val `scala3-tasty-inspector` = project.in(file("tasty-inspector")).
    withCommonSettings(Bootstrapped).
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-tasty-inspector (see sbt-dotty/sbt-test/sbt-dotty/tasty-inspector-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(commonBootstrappedSettings).
    settings(
      javaOptions := (javaOptions in `scala3-compiler-bootstrapped`).value
    )

  /** Scala library compiled by dotty using the latest published sources of the library */
  lazy val `stdlib-bootstrapped` = project.in(file("stdlib-bootstrapped")).
    withCommonSettings(Bootstrapped).
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    dependsOn(`scala3-tasty-inspector` % "test->test").
    settings(commonBootstrappedSettings).
    settings(
      moduleName := "scala-library",
      javaOptions := (javaOptions in `scala3-compiler-bootstrapped`).value,
      Compile/scalacOptions += "-Yerased-terms",
      Compile/scalacOptions ++= {
        Seq(
          "-sourcepath",
          Seq(
            (Compile/sourceManaged).value / "scala-library-src",
            (Compile/sourceManaged).value / "dotty-library-src",
          ).mkString(File.pathSeparator),
        )
      },
      scalacOptions -= "-Xfatal-warnings",
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      libraryDependencies +=
        ("org.scala-lang" % "scala-library" % stdlibVersion(Bootstrapped) % "sourcedeps"),
      sourceGenerators in Compile += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (sourceManaged in Compile).value / "scala-library-src"

        val report = updateClassifiers.value
        val scalaLibrarySourcesJar = report.select(
            configuration = configurationFilter("sourcedeps"),
            module = (_: ModuleID).name == "scala-library",
            artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
          sys.error(s"Could not fetch scala-library sources")
        }

        FileFunction.cached(cacheDir / s"fetchScalaLibrarySrc",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info(s"Unpacking scala-library sources to $trgDir...")
          if (trgDir.exists)
            IO.delete(trgDir)
          IO.createDirectory(trgDir)
          IO.unzip(scalaLibrarySourcesJar, trgDir)

          ((trgDir ** "*.scala") +++ (trgDir ** "*.java")).get.toSet
        } (Set(scalaLibrarySourcesJar)).toSeq
      }.taskValue,
      sourceGenerators in Compile += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (sourceManaged in Compile).value / "dotty-library-src"

        // NOTE `sourceDirectory` is used for actual copying,
        // but `sources` are used as cache keys
        val dottyLibSourceDirs = (`scala3-library-bootstrapped`/Compile/unmanagedSourceDirectories).value
        def dottyLibSources = dottyLibSourceDirs.foldLeft(PathFinder.empty) { (pf, dir) =>
          if (!dir.exists) pf else pf +++ (dir ** "*.scala") +++ (dir ** "*.java")
        }

        val cachedFun = FileFunction.cached(
          cacheDir / s"copyDottyLibrarySrc",
          FilesInfo.lastModified,
          FilesInfo.exists,
        ) { _ =>
          if (trgDir.exists) IO.delete(trgDir)
          dottyLibSourceDirs.foreach { dir =>
            if (dir.exists) {
              s.log.info(s"Copying scala3-library sources from $dir to $trgDir...")
              IO.copyDirectory(dir, trgDir)
            }
          }

          ((trgDir ** "*.scala") +++ (trgDir ** "*.java")).get.toSet
        }

        cachedFun(dottyLibSources.get.toSet).toSeq
      }.taskValue,
      sources in Compile ~= (_.filterNot(file =>
        // sources from https://github.com/scala/scala/tree/2.13.x/src/library-aux
        file.getPath.endsWith("scala-library-src/scala/Any.scala") ||
        file.getPath.endsWith("scala-library-src/scala/AnyVal.scala") ||
        file.getPath.endsWith("scala-library-src/scala/AnyRef.scala") ||
        file.getPath.endsWith("scala-library-src/scala/Nothing.scala") ||
        file.getPath.endsWith("scala-library-src/scala/Null.scala") ||
        file.getPath.endsWith("scala-library-src/scala/Singleton.scala"))),
      managedClasspath in Test ~= {
        _.filterNot(file => file.data.getName == s"scala-library-${stdlibVersion(Bootstrapped)}.jar")
      },
    )

  /** Test the tasty generated by `stdlib-bootstrapped`
   *
   *  The tests are run with the bootstrapped compiler and the tasty inpector on the classpath.
   *  The classpath has the default `scala-library` and not `stdlib-bootstrapped`.
   *
   *  The jar of `stdlib-bootstrapped` is provided for to the tests.
   *   - inspector: test that we can load the contents of the jar using the tasty inspector
   *   - from-tasty: test that we can recompile the contents of the jar using `dotc -from-tasty`
   */
  lazy val `stdlib-bootstrapped-tasty-tests` = project.in(file("stdlib-bootstrapped-tasty-tests")).
    withCommonSettings(Bootstrapped).
    dependsOn(`scala3-tasty-inspector` % "test->test").
    settings(commonBootstrappedSettings).
    settings(
      javaOptions := (javaOptions in `scala3-compiler-bootstrapped`).value,
      javaOptions += "-Ddotty.scala.library=" + packageBin.in(`stdlib-bootstrapped`, Compile).value.getAbsolutePath
    )

  lazy val `scala3-sbt-bridge` = project.in(file("sbt-bridge/src")).
    // We cannot depend on any bootstrapped project to compile the bridge, since the
    // bridge is needed to compile these projects.
    dependsOn(dottyDoc(NonBootstrapped) % Provided).
    settings(commonJavaSettings).
    settings(
      description := "sbt compiler bridge for Dotty",

      sources in Test := Seq(),
      scalaSource in Compile := baseDirectory.value,
      javaSource  in Compile := baseDirectory.value,
      resourceDirectory in Compile := baseDirectory.value.getParentFile / "resources",

      // Referring to the other project using a string avoids an infinite loop
      // when sbt reads the settings.
      test in Test := (test in (LocalProject("scala3-sbt-bridge-tests"), Test)).value,

      // The `newCompilerInterface` is backward compatible with the `oldCompilerInterface`
      libraryDependencies += Dependencies.newCompilerInterface % Provided
    )

  // We use a separate project for the bridge tests since they can only be run
  // with the bootstrapped library on the classpath.
  lazy val `scala3-sbt-bridge-tests` = project.in(file("sbt-bridge/test")).
    dependsOn(dottyCompiler(Bootstrapped) % Test).
    settings(commonBootstrappedSettings).
    settings(
      sources in Compile := Seq(),
      scalaSource in Test := baseDirectory.value,
      javaSource  in Test := baseDirectory.value,

      // Tests disabled until zinc-api-info cross-compiles with 2.13,
      // alternatively we could just copy in sources the part of zinc-api-info we need.
      sources in Test := Seq()
    )

  lazy val `scala3-language-server` = project.in(file("language-server")).
    dependsOn(dottyCompiler(Bootstrapped)).
    settings(commonBootstrappedSettings).
    settings(
      // Sources representing the shared configuration file used to communicate between the sbt-dotty
      // plugin and the language server
      unmanagedSourceDirectories in Compile += baseDirectory.value / "../sbt-dotty/src/dotty/tools/sbtplugin/config",

      libraryDependencies ++= Seq(
        "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.6.0",
        Dependencies.`jackson-databind`
      ),
      // Work around https://github.com/eclipse/lsp4j/issues/295
      dependencyOverrides += "org.eclipse.xtend" % "org.eclipse.xtend.lib" % "2.16.0",
      javaOptions := (javaOptions in `scala3-compiler-bootstrapped`).value,

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
      ideTestsCompilerVersion := (version in `scala3-compiler`).value,
      ideTestsCompilerArguments := Seq(),
      ideTestsDependencyClasspath := {
        val dottyLib = (classDirectory in `scala3-library-bootstrapped` in Compile).value
        val scalaLib =
          (dependencyClasspath in `scala3-library-bootstrapped` in Compile)
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
    enablePlugins(MyScalaJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(
      // Required to run Scala.js tests.
      fork in Test := false,

      scalaJSUseMainModuleInitializer := true,
    )

  /** Scala.js test suite.
   *
   *  This project downloads the sources of the upstream Scala.js test suite,
   *  and tests them with the dotty Scala.js back-end. Currently, only a very
   *  small fraction of the upstream test suite is actually compiled and run.
   *  It will grow in the future, as more stuff is confirmed to be supported.
   */
  lazy val sjsJUnitTests = project.in(file("tests/sjs-junit")).
    enablePlugins(MyScalaJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(
      scalacOptions --= Seq("-Xfatal-warnings", "-deprecation"),

      // Required to run Scala.js tests.
      fork in Test := false,

      sourceDirectory in fetchScalaJSSource := target.value / s"scala-js-src-$scalaJSVersion",

      fetchScalaJSSource := {
        import org.eclipse.jgit.api._
        import org.eclipse.jgit.lib._

        val s = streams.value
        val ver = scalaJSVersion
        val trgDir = (sourceDirectory in fetchScalaJSSource).value

        if (!trgDir.exists) {
          s.log.info(s"Fetching Scala.js source version $ver")
          IO.createDirectory(trgDir)
          new CloneCommand()
            .setDirectory(trgDir)
            .setURI("https://github.com/scala-js/scala-js.git")
            .setNoCheckout(true)
            .call()
        }

        // Checkout proper ref. We do this anyway so we fail if something is wrong
        val git = Git.open(trgDir)
        s.log.info(s"Checking out Scala.js source version $ver")
        git.getRepository().getConfig().setEnum("core", null, "autocrlf", CoreConfig.AutoCRLF.FALSE)
        git.checkout().setName(s"v$ver").call()

        trgDir
      },

      // We need JUnit in the Compile configuration
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion).withDottyCompat(scalaVersion.value),

      sourceGenerators in Compile += Def.task {
        import org.scalajs.linker.interface.CheckedBehavior

        val stage = scalaJSStage.value

        val linkerConfig = stage match {
          case FastOptStage => (scalaJSLinkerConfig in (Compile, fastOptJS)).value
          case FullOptStage => (scalaJSLinkerConfig in (Compile, fullOptJS)).value
        }

        val moduleKind = linkerConfig.moduleKind
        val sems = linkerConfig.semantics

        ConstantHolderGenerator.generate(
            (sourceManaged in Compile).value,
            "org.scalajs.testsuite.utils.BuildInfo",
            "scalaVersion" -> scalaVersion.value,
            "hasSourceMaps" -> false, //MyScalaJSPlugin.wantSourceMaps.value,
            "isNoModule" -> (moduleKind == ModuleKind.NoModule),
            "isESModule" -> (moduleKind == ModuleKind.ESModule),
            "isCommonJSModule" -> (moduleKind == ModuleKind.CommonJSModule),
            "isFullOpt" -> (stage == FullOptStage),
            "compliantAsInstanceOfs" -> (sems.asInstanceOfs == CheckedBehavior.Compliant),
            "compliantArrayIndexOutOfBounds" -> (sems.arrayIndexOutOfBounds == CheckedBehavior.Compliant),
            "compliantModuleInit" -> (sems.moduleInit == CheckedBehavior.Compliant),
            "strictFloats" -> sems.strictFloats,
            "productionMode" -> sems.productionMode,
            "es2015" -> linkerConfig.esFeatures.useECMAScript2015,
        )
      }.taskValue,

      scalacOptions in Test += "-scalajs-genStaticForwardersForNonTopLevelObjects",

      scalaJSLinkerConfig ~= { _.withSemantics(build.TestSuiteLinkerOptions.semantics _) },
      scalaJSModuleInitializers in Test ++= build.TestSuiteLinkerOptions.moduleInitializers,

      // Perform Ycheck after the Scala.js-specific transformation phases
      scalacOptions += "-Ycheck:prepjsinterop,explicitJSClasses,addLocalJSFakeNews",

      jsEnvInput in Test := {
        val resourceDir = fetchScalaJSSource.value / "test-suite/js/src/test/resources"
        val f = (resourceDir / "NonNativeJSTypeTestNatives.js").toPath
        org.scalajs.jsenv.Input.Script(f) +: (jsEnvInput in Test).value
      },

      managedSources in Compile ++= {
        val dir = fetchScalaJSSource.value
        (
          (dir / "test-suite/js/src/main/scala" ** (("*.scala": FileFilter)
            -- "Typechecking*.scala" // defines a Scala 2 macro
            )).get

          ++ (dir / "junit-async/js/src/main/scala" ** "*.scala").get
        )
      },

      // A first blacklist of tests for those that do not compile or do not link
      managedSources in Test ++= {
        val dir = fetchScalaJSSource.value / "test-suite"
        (
          (dir / "shared/src/test/scala" ** (("*.scala": FileFilter)
            -- "ReflectiveCallTest.scala" // uses many forms of structural calls that are not allowed in Scala 3 anymore
            -- "EnumerationTest.scala" // scala.Enumeration support for Scala.js is not implemented in scalac (yet)
            )).get

          ++ (dir / "shared/src/test/require-sam" ** "*.scala").get
          ++ (dir / "shared/src/test/require-jdk8" ** "*.scala").get
          ++ (dir / "shared/src/test/require-jdk7" ** "*.scala").get

          ++ (dir / "js/src/test/scala" ** (("*.scala": FileFilter)
            -- "ObjectTest.scala" // compile errors caused by #9588
            -- "StackTraceTest.scala" // would require `npm install source-map-support`
            -- "UnionTypeTest.scala" // requires the Scala 2 macro defined in Typechecking*.scala
            )).get

          ++ (dir / "js/src/test/require-2.12" ** "*.scala").get
          ++ (dir / "js/src/test/require-sam" ** "*.scala").get
          ++ (dir / "js/src/test/scala-new-collections" ** "*.scala").get
        )
      },

      // A second blacklist for tests that compile and link, but do not pass at run-time.
      // Putting them here instead of above makes sure that we do not regress on compilation+linking.
      Test / testOptions += Tests.Filter { name =>
        !Set[String](
          // Not investigated so far
          "org.scalajs.testsuite.junit.JUnitAbstractClassTestCheck",
          "org.scalajs.testsuite.junit.JUnitNamesTestCheck",
          "org.scalajs.testsuite.junit.JUnitSubClassTestCheck",
          "org.scalajs.testsuite.junit.MultiCompilationSecondUnitTestCheck",
        ).contains(name)
      }
    )

  lazy val sjsCompilerTests = project.in(file("sjs-compiler-tests")).
    dependsOn(`scala3-compiler` % "test->test").
    settings(
      commonNonBootstrappedSettings,

      // Change the baseDirectory when running the tests
      baseDirectory in Test := baseDirectory.value.getParentFile,

      javaOptions ++= (javaOptions in `scala3-compiler`).value,
      javaOptions ++= {
        val externalJSDeps = (externalDependencyClasspath in (`scala3-library-bootstrappedJS`, Compile)).value
        val dottyLibraryJSJar = (packageBin in (`scala3-library-bootstrappedJS`, Compile)).value.getAbsolutePath

        Seq(
          "-Ddotty.tests.classes.dottyLibraryJS=" + dottyLibraryJSJar,
          "-Ddotty.tests.classes.scalaJSLibrary=" + findArtifactPath(externalJSDeps, "scalajs-library_2.13"),
        )
      },
    )

  lazy val `scala3-bench` = project.in(file("bench")).asDottyBench(NonBootstrapped)
  lazy val `scala3-bench-bootstrapped` = project.in(file("bench")).asDottyBench(Bootstrapped)
  lazy val `scala3-bench-run` = project.in(file("bench-run")).asDottyBench(Bootstrapped)

  val testcasesOutputDir = taskKey[String]("Root directory where tests classses are generated")
  val testcasesSourceRoot = taskKey[String]("Root directory where tests sources are generated")
  val testDocumentationRoot = taskKey[String]("Root directory where tests documentation are stored")
  val generateSelfDocumentation = taskKey[Unit]("Generate example documentation")
  // Note: the two tasks below should be one, but a bug in Tasty prevents that
  val generateScala3Documentation = inputKey[Unit]("Generate documentation for dotty lib")
  val generateTestcasesDocumentation  = taskKey[Unit]("Generate documentation for testcases, usefull for debugging tests")
  lazy val `scala3doc` = project.in(file("scala3doc")).asScala3doc
  lazy val `scala3doc-testcases` = project.in(file("scala3doc-testcases")).asScala3docTestcases

  lazy val `scala3doc-js` = project.in(file("scala3doc-js")).asScala3docJs

  // sbt plugin to use Dotty in your own build, see
  // https://github.com/lampepfl/scala3-example-project for usage.
  lazy val `sbt-dotty` = project.in(file("sbt-dotty")).
    enablePlugins(SbtPlugin).
    settings(commonSettings).
    settings(
      name := sbtDottyName,
      version := sbtDottyVersion,
      organization := "ch.epfl.lamp",
      // Keep in sync with inject-sbt-dotty.sbt
      libraryDependencies ++= Seq(
        Dependencies.`jackson-databind`,
        Dependencies.newCompilerInterface
      ),
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config",
      sbtTestDirectory := baseDirectory.value / "sbt-test",

      // The batch mode accidentally became the default with no way to disable
      // it in sbt 1.4 (https://github.com/sbt/sbt/issues/5913#issuecomment-716003195).
      // We enable it explicitly here to make it clear that we're using it.
      scriptedBatchExecution := true,

      scriptedLaunchOpts ++= Seq(
        "-Dplugin.version=" + version.value,
        "-Dplugin.scalaVersion=" + dottyVersion,
        "-Dplugin.scalaJSVersion=" + scalaJSVersion,
        "-Dsbt.boot.directory=" + ((baseDirectory in ThisBuild).value / ".sbt-scripted").getAbsolutePath // Workaround sbt/sbt#3469
      ),
      // Pass along ivy home and repositories settings to sbt instances run from the tests
      scriptedLaunchOpts ++= {
        val repositoryPath = (io.Path.userHome / ".sbt" / "repositories").absolutePath
        s"-Dsbt.repository.config=$repositoryPath" ::
        ivyPaths.value.ivyHome.map("-Dsbt.ivy.home=" + _.getAbsolutePath).toList
      },
      scriptedBufferLog := true,
      scripted := scripted.dependsOn(
        publishLocal in `scala3-sbt-bridge`,
        publishLocal in `scala3-interfaces`,
        publishLocal in `scala3-compiler-bootstrapped`,
        publishLocal in `scala3-library-bootstrapped`,
        publishLocal in `scala3-library-bootstrappedJS`,
        publishLocal in `tasty-core-bootstrapped`,
        publishLocal in `scala3-staging`,
        publishLocal in `scala3-tasty-inspector`,
        publishLocal in `scala3-doc-bootstrapped`,
        publishLocal in `scala3doc`,
        publishLocal in `scala3-bootstrapped` // Needed because sbt currently hardcodes the dotty artifact
      ).evaluated
    )

  lazy val `vscode-dotty` = project.in(file("vscode-dotty")).
    settings(commonSettings).
    settings(
      version := "0.1.17-snapshot", // Keep in sync with package.json
      autoScalaLibrary := false,
      publishArtifact := false,
      resourceGenerators in Compile += Def.task {
        // Resources that will be copied when bootstrapping a new project
        val buildSbtFile = baseDirectory.value / "out" / "build.sbt"
        IO.write(buildSbtFile,
          s"""scalaVersion := "$publishedDottyVersion"""")
        val dottyPluginSbtFile = baseDirectory.value / "out" / "scala3-plugin.sbt"
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

  val prepareCommunityBuild = taskKey[Unit]("Publish local the compiler and the sbt plugin. Also store the versions of the published local artefacts in two files, community-build/{scala3-bootstrapped.version,sbt-dotty-sbt}.")

  lazy val `community-build` = project.in(file("community-build")).
    dependsOn(dottyLibrary(Bootstrapped)).
    settings(commonBootstrappedSettings).
    settings(
      prepareCommunityBuild := {
        (publishLocal in `scala3-sbt-bridge`).value
        (publishLocal in `scala3-interfaces`).value
        (publishLocal in `tasty-core-bootstrapped`).value
        (publishLocal in `scala3-library-bootstrapped`).value
        (publishLocal in `scala3-doc-bootstrapped`).value
        (publishLocal in `scala3-tasty-inspector`).value
        (publishLocal in `scala3doc`).value
        (publishLocal in `scala3-compiler-bootstrapped`).value
        (publishLocal in `sbt-dotty`).value
        (publishLocal in `scala3-bootstrapped`).value
        (publishLocal in `scala3-library-bootstrappedJS`).value
        // (publishLocal in `scala3-staging`).value
        val pluginText =
          s"""updateOptions in Global ~= (_.withLatestSnapshots(false))
             |addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "$sbtDottyVersion")
             |addSbtPlugin("org.scala-js" % "sbt-scalajs" % "$scalaJSVersion")""".stripMargin
        IO.write(baseDirectory.value / "sbt-dotty-sbt", pluginText)
        IO.write(baseDirectory.value / "scala3-bootstrapped.version", dottyVersion)
      },
      testOptions in Test += Tests.Argument(
        TestFrameworks.JUnit,
        "--include-categories=dotty.communitybuild.TestCategory",
        "--run-listener=dotty.communitybuild.FailureSummarizer",
      ),
      Compile/run := (Compile/run).dependsOn(prepareCommunityBuild).evaluated,
      (Test / testOnly) := ((Test / testOnly) dependsOn prepareCommunityBuild).evaluated,
      (Test / test    ) := ((Test / test    ) dependsOn prepareCommunityBuild).value,
      javaOptions ++= {
        // Propagate the ivy cache directory setting to the tests, which will
        // then propagate it further to the sbt instances they will spawn.
        val sbtProps = Option(System.getProperty("sbt.ivy.home")) match {
          case Some(ivyHome) =>
            Seq(s"-Dsbt.ivy.home=$ivyHome")
          case _ =>
            Seq()
        }
        sbtProps
      }
    )

  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    isSnapshot := version.value.contains("SNAPSHOT"),
    publishTo := sonatypePublishToBundle.value,
    publishConfiguration ~= (_.withOverwrite(true)),
    publishLocalConfiguration ~= (_.withOverwrite(true)),
    publishArtifact in Test := false,
    homepage := Some(url(dottyGithubUrl)),
    licenses += (("Apache-2.0",
      url("https://www.apache.org/licenses/LICENSE-2.0"))),
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
        email = "liu@fengy.me",
        url = url("https://fengy.me")
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

  lazy val commonDistSettings = Seq(
    packMain := Map(),
    publishArtifact := false,
    packGenerateMakefile := false,
    packExpandedClasspath := true,
    packArchiveName := "scala3-" + dottyVersion
  )

  lazy val dist = project.asDist(Bootstrapped)
    .settings(
      packResourceDir += (baseDirectory.value / "bin" -> "bin"),
    )

  implicit class ProjectDefinitions(val project: Project) extends AnyVal {

    // FIXME: we do not aggregate `bin` because its tests delete jars, thus breaking other tests
    def asDottyRoot(implicit mode: Mode): Project = project.withCommonSettings.
      aggregate(`scala3-interfaces`, dottyLibrary, dottyCompiler, tastyCore, dottyDoc, `scala3-sbt-bridge`).
      bootstrappedAggregate(`scala3-language-server`, `scala3-staging`, `scala3-tasty-inspector`,
        `scala3-library-bootstrappedJS`, scala3doc).
      dependsOn(tastyCore).
      dependsOn(dottyCompiler).
      dependsOn(dottyLibrary).
      nonBootstrappedSettings(
        addCommandAlias("run", "scala3-compiler/run"),
        // Clean everything by default
        addCommandAlias("clean", ";scala3/clean;scala3-bootstrapped/clean"),
        // `publishLocal` on the non-bootstrapped compiler does not produce a
        // working distribution (it can't in general, since there's no guarantee
        // that the non-bootstrapped library is compatible with the
        // non-bootstrapped compiler), so publish the bootstrapped one by
        // default.
        addCommandAlias("publishLocal", "scala3-bootstrapped/publishLocal"),
      ).
      settings(
        publish / skip := true
      )

    def asDottyCompiler(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(`scala3-interfaces`).
      dependsOn(dottyLibrary).
      dependsOn(tastyCore).
      settings(dottyCompilerSettings)

    def asDottyLibrary(implicit mode: Mode): Project = project.withCommonSettings.
      settings(
        libraryDependencies += "org.scala-lang" % "scala-library" % stdlibVersion
      ).
      settings(dottyLibrarySettings)

    def asTastyCore(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyLibrary).
      settings(tastyCoreSettings)

    def asTastyCoreScala2: Project = project.settings(commonScala2Settings)

    def asDottyDoc(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler, dottyCompiler % "test->test").
      settings(commonDocSettings).
      settings(dottyDocSettings)

    def asDottyBench(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler).
      settings(commonBenchmarkSettings).
      enablePlugins(JmhPlugin)

    def asScala3doc: Project = {
      def generateDocumentation(targets: String, name: String, outDir: String, ref: String, params: String = "") = Def.taskDyn {
          val projectVersion = version.value
          IO.createDirectory(file(outDir))
          val sourceLinks = "-source-links:github://lampepfl/dotty "
          val revision = s"-revision $ref -project-version $projectVersion"
          val cmd = s""" -d $outDir -project "$name" $sourceLinks $revision $params $targets"""
          run.in(Compile).toTask(cmd)
      }

      def joinProducts(products: Seq[java.io.File]): String =
        products.iterator.map(_.getAbsolutePath.toString).mkString(" ")

      val flexmarkVersion = "0.42.12"

      project.settings(commonBootstrappedSettings).
        dependsOn(`scala3-compiler-bootstrapped`).
        dependsOn(`scala3-tasty-inspector`).
        settings(
          libraryDependencies ++= Seq(
            "com.vladsch.flexmark" % "flexmark" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-html-parser" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-anchorlink" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-autolink" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-emoji" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-gfm-strikethrough" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-gfm-tables" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-gfm-tasklist" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-wikilink" % flexmarkVersion,
            "com.vladsch.flexmark" % "flexmark-ext-yaml-front-matter" % flexmarkVersion,
            "nl.big-o" % "liqp" % "0.6.7",
            "org.jsoup" % "jsoup" % "1.13.1", // Needed to process .html files for static site
            Dependencies.`jackson-dataformat-yaml`,

            "com.novocode" % "junit-interface" % "0.11" % "test",
          ),
          Test / test := (Test / test).dependsOn(compile.in(Compile).in(`scala3doc-testcases`)).value,
          testcasesOutputDir.in(Test) := joinProducts((`scala3doc-testcases`/Compile/products).value),
          testcasesSourceRoot.in(Test) := (baseDirectory.in(`scala3doc-testcases`).value / "src").getAbsolutePath.toString,
          Compile / mainClass := Some("dotty.dokka.Main"),
          baseDirectory.in(run) := baseDirectory.in(ThisBuild).value,
          generateSelfDocumentation := Def.taskDyn {
            generateDocumentation(
              classDirectory.in(Compile).value.getAbsolutePath,
              "scala3doc", "scala3doc/output/self", VersionUtil.gitHash,
              "-siteroot scala3doc/documentation -project-logo scala3doc/documentation/logo.svg " +
              "-external-mappings:" +
                ".*scala.*::scala3doc::http://dotty.epfl.ch/api/," +
                ".*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/"
            )
          }.value,

          generateScala3Documentation := Def.inputTaskDyn {
            val dottydocExtraArgs = spaceDelimited("[output]").parsed
            val dest = file(dottydocExtraArgs.headOption.getOrElse("scala3doc/output/scala3")).getAbsoluteFile
            val majorVersion = (scalaBinaryVersion in LocalProject("scala3-library-bootstrapped")).value

            val dottyJars: Seq[java.io.File] = Seq(
              (`stdlib-bootstrapped`/Compile/products).value,
              (`scala3-interfaces`/Compile/products).value,
              (`tasty-core-bootstrapped`/Compile/products).value,
            ).flatten

            val roots = joinProducts(dottyJars)

            val managedSources =
              (`stdlib-bootstrapped`/Compile/sourceManaged).value / "scala-library-src"
            val projectRoot = (ThisBuild/baseDirectory).value.toPath
            val stdLibRoot = projectRoot.relativize(managedSources.toPath.normalize())
            val docRootFile = stdLibRoot.resolve("rootdoc.txt")

            if (dottyJars.isEmpty) Def.task { streams.value.log.error("Dotty lib wasn't found") }
            else Def.task{
              IO.write(dest / "versions" / "latest-nightly-base", majorVersion)

              // This file is used by GitHub Pages when the page is available in a custom domain
              IO.write(dest / "CNAME", "dotty.epfl.ch")
            }.dependsOn(generateDocumentation(
              roots, "Scala 3", dest.getAbsolutePath, "master",
              // contains special definitions which are "transplanted" elsewhere
              // and which therefore confuse Scala3doc when accessed from this pkg
              "-skip-by-id:scala.runtime.stdLibPatches " +
              // MatchCase is a special type that represents match type cases,
              // Reflect doesn't expect to see it as a standalone definition
              // and therefore it's easier just not to document it
              "-skip-by-id:scala.runtime.MatchCase " +
              "-skip-by-regex:.+\\.internal($|\\..+) " +
              "-skip-by-regex:.+\\.impl($|\\..+) " +
              "-comment-syntax wiki -siteroot scala3doc/scala3-docs -project-logo scala3doc/scala3-docs/logo.svg " +
              "-external-mappings:.*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/ " +
              s"-source-links:$stdLibRoot=github://scala/scala/v${stdlibVersion(Bootstrapped)}#src/library " +
              s"-doc-root-content $docRootFile"
              ))
          }.evaluated,

          generateTestcasesDocumentation := Def.taskDyn {
            generateDocumentation(Build.testcasesOutputDir.in(Test).value, "Scala3doc testcases", "scala3doc/output/testcases", "master")
          }.value,

          buildInfoKeys in Test := Seq[BuildInfoKey](
            Build.testcasesOutputDir.in(Test),
            Build.testcasesSourceRoot.in(Test),
            Build.testDocumentationRoot,
          ),
          Compile / buildInfoKeys := Seq[BuildInfoKey](version),
          Compile / buildInfoPackage := "dotty.dokka",
          Compile / resourceGenerators += Def.task {
            val jsDestinationFile = (Compile / resourceManaged).value / "dotty_res" / "scripts" / "searchbar.js"
            sbt.IO.copyFile((fullOptJS in Compile in `scala3doc-js`).value.data, jsDestinationFile)
            Seq(jsDestinationFile)
          }.taskValue,
          Compile / resourceGenerators += Def.task {
            val cssDesitnationFile = (Compile / resourceManaged).value / "dotty_res" / "styles" / "scala3doc-searchbar.css"
            val cssSourceFile = (resourceDirectory in Compile in `scala3doc-js`).value / "scala3doc-searchbar.css"
            FileFunction.cached(streams.value.cacheDirectory / "css-cache") { (in: Set[File]) =>
              in.headOption.map(sbt.IO.copyFile(_, cssDesitnationFile))
              Set(cssDesitnationFile)
            }.apply(Set(cssSourceFile)).toSeq
          }.taskValue,
          testDocumentationRoot := (baseDirectory.value / "test-documentations").getAbsolutePath,
          buildInfoPackage in Test := "dotty.dokka.test",
          BuildInfoPlugin.buildInfoScopedSettings(Test),
          BuildInfoPlugin.buildInfoScopedSettings(Compile),
          BuildInfoPlugin.buildInfoDefaultSettings,
        )
    }

    def asScala3docTestcases: Project =
      project.dependsOn(`scala3-compiler-bootstrapped`).settings(commonBootstrappedSettings)

    def asScala3docJs: Project =
      project.
        enablePlugins(MyScalaJSPlugin).
        dependsOn(`scala3-library-bootstrappedJS`).
        settings(
          fork in Test := false,
          scalaJSUseMainModuleInitializer := true,
          libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.1.0").withDottyCompat(scalaVersion.value)
        )


    def asDist(implicit mode: Mode): Project = project.
      enablePlugins(PackPlugin).
      withCommonSettings.
      dependsOn(`scala3-interfaces`, dottyCompiler, dottyLibrary, tastyCore, `scala3-staging`, `scala3-tasty-inspector`, dottyDoc).
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
