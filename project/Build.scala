import java.io.File
import java.nio.file._
import Process._
import Modes._
import ScaladocGeneration._
import sbt.Keys.*
import sbt.*
import sbt.nio.FileStamper
import sbt.nio.Keys.*
import complete.DefaultParsers._
import pl.project13.scala.sbt.JmhPlugin
import pl.project13.scala.sbt.JmhPlugin.JmhKeys.Jmh
import com.gradle.develocity.agent.sbt.DevelocityPlugin.autoImport._
import com.gradle.develocity.agent.sbt.api.experimental.buildcache
import com.typesafe.sbt.packager.Keys._
import com.typesafe.sbt.packager.MappingsHelper.directory
import com.typesafe.sbt.packager.universal.UniversalPlugin
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.Universal
import com.typesafe.sbt.packager.windows.WindowsPlugin
import com.typesafe.sbt.packager.windows.WindowsPlugin.autoImport.Windows
import sbt.Package.ManifestAttributes
import sbt.PublishBinPlugin.autoImport._
import dotty.tools.sbtplugin.RepublishPlugin
import dotty.tools.sbtplugin.RepublishPlugin.autoImport._
import dotty.tools.sbtplugin.ScalaLibraryPlugin
import dotty.tools.sbtplugin.DottyJSPlugin
import dotty.tools.sbtplugin.DottyJSPlugin.autoImport._

import sbt.plugins.SbtPlugin
import sbt.ScriptedPlugin.autoImport._
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport._
import sbttastymima.TastyMiMaPlugin
import sbttastymima.TastyMiMaPlugin.autoImport._

import scala.util.Properties.isJavaAtLeast

import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

import sbt.dsl.LinterLevel.Ignore

object Build {
  import ScaladocConfigs._

  /** Version of the Scala compiler used to build the artifacts.
   *  Reference version should track the latest version pushed to Maven:
   *  - In main branch it should be the last RC version
   *  - In release branch it should be the last stable release
   *
   *  Warning: Change of this variable needs to be consulted with `expectedTastyVersion`
   */
  val referenceVersion = "3.7.4-RC1"

  /** Version of the Scala compiler targeted in the current release cycle
   *  Contains a version without RC/SNAPSHOT/NIGHTLY specific suffixes
   *  Should be updated ONLY after release or cutoff for previous release cycle.
   *
   *  Should only be referred from `dottyVersion` or settings/tasks requiring simplified version string,
   *  eg. `compatMode` or Windows native distribution version.
   *
   *  Warning: Change of this variable might require updating `expectedTastyVersion`
   */
  val developedVersion = "3.8.0"

  /** The version of the compiler including the RC prefix.
   *  Defined as common base before calculating environment specific suffixes in `dottyVersion`
   *
   *  By default, during development cycle defined as `${developedVersion}-RC1`;
   *  During release candidate cycle incremented by the release officer before publishing a subsequent RC version;
   *  During final, stable release is set exactly to `developedVersion`.
  */
  val baseVersion = s"$developedVersion-RC1"

  /** The version of TASTY that should be emitted, checked in runtime test
   *  For defails on how TASTY version should be set see related discussions:
   *    - https://github.com/scala/scala3/issues/13447#issuecomment-912447107
   *    - https://github.com/scala/scala3/issues/14306#issuecomment-1069333516
   *    - https://github.com/scala/scala3/pull/19321
   *
   *  Simplified rules, given 3.$minor.$patch = $developedVersion
   *    - Major version is always 28
   *    - TASTY minor version:
   *      - in main (NIGHTLY): {if $patch == 0 || ${referenceVersion.matches(raw"3.$minor.0-RC\d")} then $minor else ${minor + 1}}
   *      - in release branch is always equal to $minor
   *    - TASTY experimental version:
   *      - in main (NIGHTLY) is always experimental
   *      - in release candidate branch is experimental if {patch == 0}
   *      - in stable release is always non-experimetnal
   */
  val expectedTastyVersion = "28.8-experimental-1"
  checkReleasedTastyVersion()

  /** Final version of Scala compiler, controlled by environment variables. */
  val dottyVersion = {
    if (isRelease) baseVersion
    else if (isNightly) {
      val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")
      val currentDate =
        formatter.format(java.time.ZonedDateTime.now(java.time.ZoneId.of("UTC")))
      s"${baseVersion}-bin-${currentDate}-${VersionUtil.gitHash}-NIGHTLY"
    }
    else s"${baseVersion}-bin-SNAPSHOT"
  }
  def isRelease = sys.env.get("RELEASEBUILD").contains("yes")
  def isNightly = sys.env.get("NIGHTLYBUILD").contains("yes")

  /** Version calculate for `nonbootstrapped` projects */
  val dottyNonBootstrappedVersion = {
    // Make sure sbt always computes the scalaBinaryVersion correctly
    val bin = if (!dottyVersion.contains("-bin")) "-bin" else ""
    dottyVersion + bin + "-nonbootstrapped"
  }

  // LTS or Next
  val versionLine = "Next"

  // Versions used by the vscode extension to create a new project
  // This should be the latest published releases.
  // TODO: Have the vscode extension fetch these numbers from the Internet
  // instead of hardcoding them ?
  val publishedDottyVersion = referenceVersion
  val sbtDottyVersion = "0.5.5"

  /** Minor version against which we check binary compatibility.
   *
   *  This must be the earliest published release in the same versioning line.
   *  For a developedVersion `3.M.P` the mimaPreviousDottyVersion should be set to:
   *   - `3.M.0`     if `P > 0`
   *   - `3.(M-1).0` if `P = 0`
   */
  val mimaPreviousDottyVersion = "3.7.3" // for 3.8.0, we compare against 3.7.3

  /** LTS version against which we check binary compatibility.
   *
   *  This must be the earliest published release in the LTS versioning line.
   *  For example, if the latest LTS release is be 3.3.4, then this must be
   *  set to 3.3.0.
   */
  val mimaPreviousLTSDottyVersion = "3.3.0"

  /** Version of Scala CLI to download */
  val scalaCliLauncherVersion = "1.9.1"
  /** Version of Coursier to download for initializing the local maven repo of Scala command */
  val coursierJarVersion = "2.1.24"

  object CompatMode {
    final val BinaryCompatible = 0
    final val SourceAndBinaryCompatible = 1
  }

  val compatMode = {
    val VersionRE = """^\d+\.(\d+)\.(\d+)""".r
    developedVersion match {
      case VersionRE(_, "0")   => CompatMode.BinaryCompatible
      case _                   => CompatMode.SourceAndBinaryCompatible
    }
  }

  /** scala-library version required to compile Dotty.
   *
   *  Both the non-bootstrapped and bootstrapped version should match, unless
   *  we're in the process of upgrading to a new major version of
   *  scala-library.
   */
  def stdlibVersion(implicit mode: Mode): String = mode match {
    case NonBootstrapped => "2.13.16"
    case Bootstrapped => "2.13.16"
  }

  /** Version of the scala-library for which we will generate TASTy.
   *
   *  We should never use a nightly version here to release.
   *
   *  We can use nightly versions to tests the future compatibility in development.
   *  Nightly versions: https://scala-ci.typesafe.com/ui/native/scala-integration/org/scala-lang
   */
  val stdlibBootstrappedVersion = "2.13.16"

  val dottyOrganization = "org.scala-lang"
  val dottyGithubUrl = "https://github.com/scala/scala3"
  val dottyGithubRawUserContentUrl = "https://raw.githubusercontent.com/scala/scala3"

  val sbtCommunityBuildVersion = "0.1.0-SNAPSHOT"

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

  // Used to compile files similar to ./bin/scalac script
  val scalac = inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

  // Used to run binaries similar to ./bin/scala script
  val scala = inputKey[Unit]("run compiled binary using the correct classpath, or the user supplied classpath")

  val repl = taskKey[Unit]("spawns a repl with the correct classpath")

  val buildQuick = taskKey[Unit]("builds the compiler and writes the classpath to bin/.cp to enable the bin/scalacQ and bin/scalaQ scripts")

  // Compiles the documentation and static site
  val genDocs = inputKey[Unit]("run scaladoc to generate static documentation site")

  // Settings used to configure the test language server
  val ideTestsCompilerVersion = taskKey[String]("Compiler version to use in IDE tests")
  val ideTestsCompilerArguments = taskKey[Seq[String]]("Compiler arguments to use in IDE tests")
  val ideTestsDependencyClasspath = taskKey[Seq[File]]("Dependency classpath to use in IDE tests")

  val fetchScalaJSSource = taskKey[File]("Fetch the sources of Scala.js")

  val extraDevelocityCacheInputFiles = taskKey[Seq[Path]]("Extra input files for caching")

  lazy val SourceDeps = config("sourcedeps")

  // Settings shared by the build (scoped in ThisBuild). Used in build.sbt
  lazy val thisBuildSettings = Def.settings(
    organization := dottyOrganization,
    organizationName := "LAMP/EPFL",
    organizationHomepage := Some(url("http://lamp.epfl.ch")),

    // Note: bench/profiles/projects.yml should be updated accordingly.
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      //"-Wconf:cat=deprecation&msg=Unsafe:s",    // example usage
      "-Werror",
      //"-Wunused:all",
      //"-rewrite", // requires -Werror:false since no rewrites are applied with errors
      "-encoding", "UTF8",
      "-language:implicitConversions",
    ),

    (Compile / compile / javacOptions) ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    // Avoid various sbt craziness involving classloaders and parallelism
    run / fork := true,
    run / connectInput := true,
    Test / fork := true,
    Test / parallelExecution := false,

    outputStrategy := Some(StdoutOutput),

    // enable verbose exception messages for JUnit
    (Test / testOptions) += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s"),

    // Configuration to publish build scans to develocity.scala-lang.org
    develocityConfiguration := {
      val isInsideCI = insideCI.value
      val config = develocityConfiguration.value
      val buildScan = config.buildScan
      val buildCache = config.buildCache
      // disable test retry on compilation test classes
      val noRetryTestClasses = Set(
        "dotty.tools.dotc.BestEffortOptionsTests",
        "dotty.tools.dotc.CompilationTests",
        "dotty.tools.dotc.FromTastyTests",
        "dotty.tools.dotc.IdempotencyTests",
        "dotty.tools.dotc.ScalaJSCompilationTests",
        "dotty.tools.dotc.TastyBootstrapTests",
        "dotty.tools.dotc.coverage.CoverageTests",
        "dotty.tools.dotc.transform.PatmatExhaustivityTest",
        "dotty.tools.repl.ScriptedTests"
      )
      config
        .withProjectId(ProjectId("scala3"))
        .withServer(config.server.withUrl(Some(url("https://develocity.scala-lang.org"))))
        .withBuildScan({
          val scan = buildScan
            .withPublishing(Publishing.onlyIf(_.authenticated))
            .withBackgroundUpload(!isInsideCI)
            .withObfuscation(buildScan.obfuscation.withIpAddresses(_.map(_ => "0.0.0.0")))
          if (isNightly) scan.withTag("NIGHTLY") else scan
        })
        .withBuildCache(
          buildCache
            .withLocal(buildCache.local.withEnabled(true).withStoreEnabled(true))
            .withRemote(buildCache.remote.withEnabled(true).withStoreEnabled(isInsideCI))
            .withRequireClean(!isInsideCI)
        )
        .withTestRetry(
          config.testRetry
            .withFlakyTestPolicy(FlakyTestPolicy.Fail)
            .withMaxRetries(if (isInsideCI) 1 else 0)
            .withMaxFailures(10)
            .withClassesFilter((className, _) => !noRetryTestClasses.contains(className))
        )
    },
    // Deactivate Develocity's test caching because it caches all tests or nothing.
    // Also at the moment, it does not take compilation files as inputs.
    Test / develocityBuildCacheClient := None,
    extraDevelocityCacheInputFiles := Seq.empty,
    extraDevelocityCacheInputFiles / outputFileStamper := FileStamper.Hash,
    resolvers += ("Artifactory" at "https://repo.scala-lang.org/artifactory/fat-jar/"),
  )

  // Settings shared globally (scoped in Global). Used in build.sbt
  lazy val globalSettings = Def.settings(
    onLoad := (Global / onLoad).value andThen { state =>
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
      } yield Credentials("Sonatype Nexus Repository Manager", "central.sonatype.com", username, password)
    ).toList,

    // Do not cut off the bottom of large stack traces (default is 1024)
    javaOptions ++= "-XX:MaxJavaStackTraceDepth=1000000" :: agentOptions,

    excludeLintKeys ++= Set(
      // We set these settings in `commonSettings`, if a project
      // uses `commonSettings` but overrides `unmanagedSourceDirectories`,
      // sbt will complain if we don't exclude them here.
      Keys.scalaSource, Keys.javaSource
    ),
  )

  lazy val disableDocSetting =
      // This is a legacy settings, we should reevalute generating javadocs
      Compile / doc / sources := Seq()

  lazy val commonSettings = publishSettings ++ Seq(
    (Compile / scalaSource)    := baseDirectory.value / "src",
    (Test / scalaSource)       := baseDirectory.value / "test",
    (Compile / javaSource)    := baseDirectory.value / "src",
    (Test / javaSource)       := baseDirectory.value / "test",
    (Compile / resourceDirectory)    := baseDirectory.value / "resources",
    (Test / resourceDirectory)       := baseDirectory.value / "test-resources",

    // Prevent sbt from rewriting our dependencies
    scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(false))),

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test,

    // If someone puts a source file at the root (e.g., for manual testing),
    // don't pick it up as part of any project.
    sourcesInBase := false,

    // For compatibility with Java 9+ module system;
    // without Automatic-Module-Name, the module name is derived from the jar file which is invalid because of the _3 suffix.
    Compile / packageBin / packageOptions +=
      Package.ManifestAttributes(
        "Automatic-Module-Name" -> s"${dottyOrganization.replaceAll("-",".")}.${moduleName.value.replaceAll("-",".")}"
      ),

    // add extraDevelocityCacheInputFiles in cache key components
    Compile / compile / buildcache.develocityTaskCacheKeyComponents +=
      (Compile / extraDevelocityCacheInputFiles / outputFileStamps).taskValue,
    Test / test / buildcache.develocityTaskCacheKeyComponents +=
      (Test / extraDevelocityCacheInputFiles / outputFileStamps).taskValue,
    Test / testOnly / buildcache.develocityInputTaskCacheKeyComponents +=
      (Test / extraDevelocityCacheInputFiles / outputFileStamps).taskValue,
    Test / testQuick / buildcache.develocityInputTaskCacheKeyComponents +=
      (Test / extraDevelocityCacheInputFiles / outputFileStamps).taskValue
  )

  // Settings used for projects compiled only with Java
  lazy val commonJavaSettings = commonSettings ++ Seq(
    version := dottyVersion,
    scalaVersion := referenceVersion,
    // Do not append Scala versions to the generated artifacts
    crossPaths := false,
    // Do not depend on the Scala library
    autoScalaLibrary := false,
    disableDocSetting
  )

  // Settings used when compiling dotty (both non-bootstrapped and bootstrapped)
  lazy val commonDottySettings = commonSettings ++ Seq(
    // Manually set the standard library to use
    autoScalaLibrary := false,
    classpathOptions ~= (old =>
      old
        .withAutoBoot(false)      // no library on the compiler bootclasspath - we may need a more recent version
        .withFilterLibrary(false) // ...instead, we put it on the compiler classpath
    ),
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
    (Compile / unmanagedSourceDirectories) += baseDirectory.value / "src-non-bootstrapped",

    version := dottyNonBootstrappedVersion,
    scalaVersion := referenceVersion,

    disableDocSetting
  )

  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  def scalacOptionsDocSettings(includeExternalMappings: Boolean = true) = {
    val extMap = Seq("-external-mappings:" +
        (if (includeExternalMappings) ".*scala/.*::scaladoc3::https://dotty.epfl.ch/api/," else "") +
        ".*java/.*::javadoc::https://docs.oracle.com/javase/8/docs/api/")
    Seq(
      "-skip-by-regex:.+\\.internal($|\\..+)",
      "-skip-by-regex:.+\\.impl($|\\..+)",
      "-project-logo", "docs/_assets/images/logo.svg",
      "-social-links:" +
        "github::https://github.com/scala/scala3," +
        "discord::https://discord.com/invite/scala," +
        "twitter::https://twitter.com/scala_lang",
      // contains special definitions which are "transplanted" elsewhere
      // and which therefore confuse Scaladoc when accessed from this pkg
      "-skip-by-id:scala.runtime.stdLibPatches",
      // MatchCase is a special type that represents match type cases,
      // Reflect doesn't expect to see it as a standalone definition
      // and therefore it's easier just not to document it
      "-skip-by-id:scala.runtime.MatchCase",
      "-skip-by-id:dotty.tools.tasty",
      "-skip-by-id:dotty.tools.tasty.util",
      "-skip-by-id:dotty.tools.tasty.besteffort",
      "-project-footer", s"Copyright (c) 2002-$currentYear, LAMP/EPFL",
      "-author",
      "-groups",
      "-default-template", "static-site-main"
    ) ++ extMap
  }

  val enableBspAllProjects = sys.env.get("ENABLE_BSP_ALL_PROJECTS").map(_.toBoolean).getOrElse{
    val enableBspAllProjectsFile = file(".enable_bsp_all_projects")
    enableBspAllProjectsFile.exists()
  }

  // Settings used when compiling dotty with a non-bootstrapped dotty
  lazy val commonBootstrappedSettings = commonDottySettings ++ Seq(
    // To enable support of scaladoc and language-server projects you need to change this to true
    bspEnabled := enableBspAllProjects,
    (Compile / unmanagedSourceDirectories) += baseDirectory.value / "src-bootstrapped",

    version := dottyVersion,
    scalaVersion := dottyNonBootstrappedVersion,

    scalaCompilerBridgeBinaryJar := {
      Some((`scala3-sbt-bridge` / Compile / packageBin).value)
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
      val externalLibraryDeps = (`scala3-library` / Compile / externalDependencyClasspath).value.map(_.data).toSet
      val externalCompilerDeps = (`scala3-compiler` / Compile / externalDependencyClasspath).value.map(_.data).toSet

      // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
      // just directories containing classfiles because sbt maintains a cache of
      // compiler instances. This cache is invalidated based on timestamps
      // however this is only implemented on jars, directories are never
      // invalidated.
      val tastyCore = (`tasty-core` / Compile / packageBin).value
      val scala3Library = (`scala3-library` / Compile / packageBin).value
      val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
      val scala3Compiler = (`scala3-compiler` / Compile / packageBin).value

      val libraryJars = Array(scala3Library) ++ externalLibraryDeps
      val compilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ (externalCompilerDeps -- externalLibraryDeps)

      Defaults.makeScalaInstance(
        scalaVersion.value,
        libraryJars = libraryJars,
        allCompilerJars = compilerJars,
        allDocJars = Seq.empty,
        state.value,
        scalaInstanceTopLoader.value
      )
    },
    // We cannot include scaladoc in the regular `scalaInstance` task because
    // it's a bootstrapped-only project, so we would run into a loop since we
    // need the output of that task to compile scaladoc. But we can include it
    // in the `scalaInstance` of the `doc` task which allows us to run
    // `scala3-library-bootstrapped/doc` for example.
    doc / scalaInstance := {
      val externalDeps = (LocalProject("scaladoc") / Compile / externalDependencyClasspath).value.map(_.data)
      val scalaDoc = (LocalProject("scaladoc") / Compile / packageBin).value
      val docJars = Array(scalaDoc) ++ externalDeps

      val base = scalaInstance.value
      val docScalaInstance = Defaults.makeScalaInstance(
        version = base.version,
        libraryJars = base.libraryJars,
        allCompilerJars = base.compilerJars,
        allDocJars = docJars,
        state.value,
        scalaInstanceTopLoader.value
      )
      // assert that sbt reuses the same compiler class loader
      assert(docScalaInstance.loaderCompilerOnly == base.loaderCompilerOnly)
      docScalaInstance
    },
    Compile / doc / scalacOptions ++= scalacOptionsDocSettings(),
    // force recompilation of bootstrapped modules when the compiler changes
    Compile / extraDevelocityCacheInputFiles ++=
      (`scala3-compiler` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
  )

  lazy val commonBenchmarkSettings = Seq(
    Jmh / bspEnabled := false,
    Jmh / run / mainClass := Some("dotty.tools.benchmarks.Bench"), // custom main for jmh:run
    javaOptions += "-DBENCH_COMPILER_CLASS_PATH=" + Attributed.data((`scala3-bootstrapped` / Compile / fullClasspath).value).mkString("", File.pathSeparator, ""),
    javaOptions += "-DBENCH_CLASS_PATH=" + Attributed.data((`scala3-library-bootstrapped` / Compile / fullClasspath).value).mkString("", File.pathSeparator, "")
  )

  lazy val commonMiMaSettings = Def.settings(
    mimaPreviousArtifacts += {
      val thisProjectID = projectID.value
      val crossedName = thisProjectID.crossVersion match {
        case cv: Disabled => thisProjectID.name
        case cv: Binary => s"${thisProjectID.name}_${cv.prefix}3${cv.suffix}"
      }
      (thisProjectID.organization % crossedName % mimaPreviousDottyVersion)
    },

    mimaCheckDirection := (compatMode match {
      case CompatMode.BinaryCompatible          => "backward"
      case CompatMode.SourceAndBinaryCompatible => "both"
    }),

    mimaExcludeAnnotations += "scala.annotation.experimental",
  )

  /** Projects -------------------------------------------------------------- */

  val dottyCompilerBootstrappedRef = LocalProject("scala3-compiler-bootstrapped")

  /** External dependencies we may want to put on the compiler classpath. */
  def externalCompilerClasspathTask: Def.Initialize[Task[Def.Classpath]] =
    // Even if we're running the non-bootstrapped compiler, we want the
    // dependencies of the bootstrapped compiler since we want to put them on
    // the compiler classpath, not the JVM classpath.
    (dottyCompilerBootstrappedRef / Runtime / externalDependencyClasspath)

  // The root project:
  // - aggregates other projects so that "compile", "test", etc are run on all projects at once.
  // - publishes its own empty artifact "dotty" that depends on "scala3-library" and "scala3-compiler",
  //   this is only necessary for compatibility with sbt which currently hardcodes the "dotty" artifact name
  lazy val scala3 = project.in(file(".")).asDottyRoot(NonBootstrapped)
  lazy val `scala3-bootstrapped` = project.asDottyRoot(Bootstrapped)

  lazy val `scala3-interfaces` = project.in(file("interfaces")).
    settings(commonJavaSettings).
    settings(commonMiMaSettings).
    settings(
      versionScheme := Some("semver-spec"),
      mimaForwardIssueFilters := MiMaFilters.Interfaces.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.Interfaces.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.Interfaces"),
    )

  /** Find an artifact with the given `name` in `classpath` */
  def findArtifact(classpath: Def.Classpath, name: String): File = classpath
    .find(_.get(artifact.key).exists(_.name == name))
    .getOrElse(throw new MessageOnlyException(s"Artifact for $name not found in $classpath"))
    .data

  /** Like `findArtifact` but returns the absolute path of the entry as a string */
  def findArtifactPath(classpath: Def.Classpath, name: String): String =
    findArtifact(classpath, name).getAbsolutePath

  /** Replace package names in package definitions, for shading.
   * It assumes the full package def is written on a single line.
   * It does not adapt the imports accordingly.
   */
  def replacePackage(lines: List[String])(replace: PartialFunction[String, String]): List[String] = {
    def recur(lines: List[String]): List[String] =
      lines match {
        case head :: tail =>
          if (head.startsWith("package ")) {
            val packageName = head.stripPrefix("package ").trim
            val newPackageName = replace.applyOrElse(packageName, (_: String) => packageName)
            s"package $newPackageName" :: tail
          } else head :: recur(tail)
        case _ => lines
      }
    recur(lines)
  }

  /** Insert UnsafeNulls Import after package */
  def insertUnsafeNullsImport(lines: List[String]): List[String] = {
    def recur(ls: List[String], foundPackage: Boolean): List[String] = ls match {
      case l :: rest =>
        val lt = l.trim()
        if (foundPackage) {
          if (!(lt.isEmpty || lt.startsWith("package ")))
            "import scala.language.unsafeNulls" :: ls
          else l :: recur(rest, foundPackage)
        } else {
          if (lt.startsWith("package ")) l +: recur(rest, true)
          else l :: recur(rest, foundPackage)
        }
      case _ => ls
    }
    recur(lines, false)
  }

  /** replace imports of `com.google.protobuf.*` with compiler implemented version */
  def replaceProtobuf(lines: List[String]): List[String] = {
    def recur(ls: List[String]): List[String] = ls match {
      case l :: rest =>
        val lt = l.trim()
        if (lt.isEmpty || lt.startsWith("package ") || lt.startsWith("import ")) {
          val newLine =
            if (lt.startsWith("import com.google.protobuf.")) {
              if (lt == "import com.google.protobuf.CodedInputStream") {
                "import dotty.tools.dotc.semanticdb.internal.SemanticdbInputStream as CodedInputStream"
              } else if (lt == "import com.google.protobuf.CodedOutputStream") {
                "import dotty.tools.dotc.semanticdb.internal.SemanticdbOutputStream as CodedOutputStream"
              } else {
                l
              }
            } else {
              l
            }
          newLine :: recur(rest)
        } else {
          ls // don't check rest of file
        }
      case _ => ls
    }
    recur(lines)
  }

  // Settings shared between scala3-compiler and scala3-compiler-bootstrapped
  lazy val commonDottyCompilerSettings = Seq(
      // Note: bench/profiles/projects.yml should be updated accordingly.
      Compile / scalacOptions ++= Seq("-Yexplicit-nulls", "-Wsafe-init"),

      // Use source 3.3 to avoid fatal migration warnings on scalajs-ir
      scalacOptions ++= Seq("-source", "3.3"),

      /* Ignore a deprecation warning about AnyRefMap in scalajs-ir. The latter
       * cross-compiles for 2.12, and therefore AnyRefMap remains useful there
       * for performance reasons.
       * The build of Scala.js core does the same thing.
       */
      scalacOptions += "-Wconf:cat=deprecation&origin=scala\\.collection\\.mutable\\.AnyRefMap.*:s",

      // Generate compiler.properties, used by sbt
      (Compile / resourceGenerators) += Def.task {
        import java.util._
        import java.text._
        val file = (Compile / resourceManaged).value / "compiler.properties"
        val dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss")
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"))
        val contents =                //2.11.11.v20170413-090219-8a413ba7cc
          s"""version.number=${version.value}
             |maven.version.number=${version.value}
             |git.hash=${VersionUtil.gitHash}
             |copyright.string=Copyright 2002-$currentYear, LAMP/EPFL
           """.stripMargin

        if (!(file.exists && IO.read(file) == contents)) {
          IO.write(file, contents)
        }

        Seq(file)
      }.taskValue,

      // get libraries onboard
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" % "scala-asm" % "9.8.0-scala-1", // used by the backend
        Dependencies.compilerInterface,
        "org.jline" % "jline-reader" % "3.29.0",   // used by the REPL
        "org.jline" % "jline-terminal" % "3.29.0",
        "org.jline" % "jline-terminal-jni" % "3.29.0", // needed for Windows
        ("io.get-coursier" %% "coursier" % "2.0.16" % Test).cross(CrossVersion.for3Use2_13),
      ),

      (Compile / sourceGenerators) += ShadedSourceGenerator.task.taskValue,

      // For convenience, change the baseDirectory when running the compiler
      Compile / forkOptions := (Compile / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
      Compile / run / forkOptions := (Compile / run / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
      // And when running the tests
      Test / forkOptions := (Test / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),

      Test / test := {
        // Exclude VulpixMetaTests
        (Test / testOnly).toTask(" -- --exclude-categories=dotty.VulpixMetaTests").value
      },

      (Test / testOptions) += Tests.Argument(
        TestFrameworks.JUnit,
        "--run-listener=dotty.tools.ContextEscapeDetector",
      ),

      // Spawn new JVM in run and test

      // Add git-hash used to package the distribution to the manifest to know it in runtime and report it in REPL
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)),

      javaOptions ++= {
        val log = streams.value.log
        val managedSrcDir = {
          // Populate the directory
          (Compile / managedSources).value

          (Compile / sourceManaged).value
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

      javaOptions ++= Seq(
        s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
      ),

      testCompilation := Def.inputTaskDyn {
        val args = spaceDelimited("<arg>").parsed
        if (args.contains("--help")) {
          println(
            s"""
               |usage: testCompilation [--help] [--from-tasty] [--update-checkfiles] [--failed] [<filter>]
               |
               |By default runs tests in dotty.tools.dotc.*CompilationTests and dotty.tools.dotc.coverage.*,
               |excluding tests tagged with dotty.SlowTests.
               |
               |  --help                show this message
               |  --from-tasty          runs tests in dotty.tools.dotc.FromTastyTests
               |  --update-checkfiles   override the checkfiles that did not match with the current output
               |  --failed              re-run only failed tests
               |  <filter>              substring of the path of the tests file
               |
             """.stripMargin
          )
          (Test / testOnly).toTask(" not.a.test")
        }
        else {
          val updateCheckfile = args.contains("--update-checkfiles")
          val rerunFailed = args.contains("--failed")
          val fromTasty = args.contains("--from-tasty")
          val args1 = if (updateCheckfile | fromTasty | rerunFailed) args.filter(x => x != "--update-checkfiles" && x != "--from-tasty" && x != "--failed") else args
          val test = if (fromTasty) "dotty.tools.dotc.FromTastyTests" else "dotty.tools.dotc.*CompilationTests dotty.tools.dotc.coverage.*"
          val cmd = s" $test -- --exclude-categories=dotty.SlowTests" +
            (if (updateCheckfile) " -Ddotty.tests.updateCheckfiles=TRUE" else "") +
            (if (rerunFailed) " -Ddotty.tests.rerunFailed=TRUE" else "") +
            (if (args1.nonEmpty) " -Ddotty.tests.filter=" + args1.mkString(" ") else "")
          (Test / testOnly).toTask(cmd)
        }
      }.evaluated,

      Compile / mainClass := Some("dotty.tools.dotc.Main"),

      scala := {
        val args: List[String] = spaceDelimited("<arg>").parsed.toList
        val externalDeps = externalCompilerClasspathTask.value
        val jars = packageAll.value

        val argsWithDefaultClasspath: List[String] =
          if (args.contains("-classpath")) args
          else insertClasspathInArgs(args, (baseDirectory.value / ".." / "out" / "default-last-scalac-out.jar").getPath)

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
          val args1 = argsWithDefaultClasspath.filter(_ != "-with-compiler")
          val asm = findArtifactPath(externalDeps, "scala-asm")
          val dottyCompiler = jars("scala3-compiler")
          val dottyStaging = jars("scala3-staging")
          val dottyTastyInspector = jars("scala3-tasty-inspector")
          val dottyInterfaces = jars("scala3-interfaces")
          val tastyCore = jars("tasty-core")
          val compilerInterface = findArtifactPath(externalDeps, "compiler-interface")
          run(insertClasspathInArgs(args1, List(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore, compilerInterface).mkString(File.pathSeparator)))
        } else run(argsWithDefaultClasspath)
      },

      run := scalac.evaluated,
      scalac := Def.inputTaskDyn {
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
        val defaultOutputDirectory =
          if (printTasty || decompile || debugFromTasty || args0.contains("-d")) Nil else List("-d", (baseDirectory.value / ".." / "out" / "default-last-scalac-out.jar").getPath)
        val args = args0.filter(arg => arg != "-repl" && arg != "-decompile" &&
            arg != "-with-compiler" && arg != "-Ythrough-tasty" && arg != "-print-tasty")
        val main =
          if (decompile) "dotty.tools.dotc.decompiler.Main"
          else if (printTasty) "dotty.tools.dotc.core.tasty.TastyPrinter"
          else if (debugFromTasty) "dotty.tools.dotc.fromtasty.Debug"
          else "dotty.tools.dotc.Main"

        var extraClasspath = Seq(scalaLib, dottyLib)

        if (decompile && !args.contains("-classpath"))
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
          val compilerInterface = findArtifactPath(externalDeps, "compiler-interface")
          extraClasspath ++= Seq(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore, compilerInterface)
        }

        val wrappedArgs = if (printTasty) args else insertClasspathInArgs(args, extraClasspath.mkString(File.pathSeparator))
        val fullArgs = main :: (defaultOutputDirectory ::: wrappedArgs).map("\""+ _ + "\"").map(_.replace("\\", "\\\\"))

        (Compile / runMain).toTask(fullArgs.mkString(" ", " ", ""))
      }.evaluated,

      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps").cross(CrossVersion.for3Use2_13),
      (Compile / sourceGenerators) += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (Compile / sourceManaged).value / "scalajs-ir-src"

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

          val sjsSources = (trgDir ** "*.scala").get.toSet
          sjsSources.foreach(f => {
            val lines = IO.readLines(f)
            val linesWithPackage = replacePackage(lines) {
              case "org.scalajs.ir" => "dotty.tools.sjs.ir"
            }
            IO.writeLines(f, insertUnsafeNullsImport(linesWithPackage))
          })
          sjsSources
        } (Set(scalaJSIRSourcesJar)).toSeq
      }.taskValue,
  )

  def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + File.pathSeparator + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }

  lazy val nonBootstrappedDottyCompilerSettings = commonDottyCompilerSettings ++ Seq(
    // packageAll packages all and then returns a map with the abs location
    packageAll := Def.taskDyn { // Use a dynamic task to avoid loops when loading the settings
      Def.task {
        Map(
          "scala3-interfaces"    -> (`scala3-interfaces` / Compile / packageBin).value,
          "scala3-compiler"      -> (Compile / packageBin).value,
          "tasty-core"          -> (`tasty-core` / Compile / packageBin).value,

          // NOTE: Using scala3-library-bootstrapped here is intentional: when
          // running the compiler, we should always have the bootstrapped
          // library on the compiler classpath since the non-bootstrapped one
          // may not be binary-compatible.
          "scala3-library"       -> (`scala3-library-bootstrapped` / Compile / packageBin).value
        ).mapValues(_.getAbsolutePath)
      }
    }.value,

    (Test / testOptions) += Tests.Argument(
      TestFrameworks.JUnit,
      "--exclude-categories=dotty.BootstrappedOnlyTests",
    ),
    // increase stack size for non-bootstrapped compiler, because some code
    // is only tail-recursive after bootstrap
    (Test / javaOptions) += "-Xss2m"
  )

  lazy val bootstrappedDottyCompilerSettings = commonDottyCompilerSettings ++ Seq(
    javaOptions ++= {
      val jars = packageAll.value
      Seq(
        "-Ddotty.tests.classes.dottyStaging=" + jars("scala3-staging"),
        "-Ddotty.tests.classes.dottyTastyInspector=" + jars("scala3-tasty-inspector"),
      )
    },
    // For compatibility at this moment, both the bootstrapped and the non-bootstrapped
    // compilers are compiled without flexible types.
    // We should move the flag to commonDottyCompilerSettings once the reference
    // compiler is updated.
    // Then, the next step is to enable flexible types by default and reduce the use of
    // `unsafeNulls`.
    packageAll := {
      (`scala3-compiler` / packageAll).value ++ Seq(
        "scala3-compiler" -> (Compile / packageBin).value.getAbsolutePath,
        "scala3-staging"  -> (LocalProject("scala3-staging") / Compile / packageBin).value.getAbsolutePath,
        "scala3-tasty-inspector"  -> (LocalProject("scala3-tasty-inspector") / Compile / packageBin).value.getAbsolutePath,
        "tasty-core"     -> (LocalProject("tasty-core-bootstrapped") / Compile / packageBin).value.getAbsolutePath,
      )
    },

    repl := (Compile / console).value,
    Compile / console / scalacOptions := Nil, // reset so that we get stock REPL behaviour!  E.g. avoid -unchecked being enabled
  )

  def dottyCompilerSettings(implicit mode: Mode): sbt.Def.SettingsDefinition =
    if (mode == NonBootstrapped) nonBootstrappedDottyCompilerSettings else bootstrappedDottyCompilerSettings

  lazy val `scala3-compiler` = project.in(file("compiler")).asDottyCompiler(NonBootstrapped)

  lazy val Scala3CompilerCoursierTest = config("scala3CompilerCoursierTest") extend Test
  lazy val `scala3-compiler-bootstrapped` = project.in(file("compiler")).asDottyCompiler(Bootstrapped)
    .configs(Scala3CompilerCoursierTest)
    .settings(
      inConfig(Scala3CompilerCoursierTest)(Defaults.testSettings),
      Scala3CompilerCoursierTest / scalaSource := baseDirectory.value / "test-coursier",
      Scala3CompilerCoursierTest / fork := true,
      Scala3CompilerCoursierTest / envVars := Map("DOTTY_BOOTSTRAPPED_VERSION" -> dottyVersion),
      Scala3CompilerCoursierTest / unmanagedClasspath += (Scala3CompilerCoursierTest / scalaSource).value,
      Scala3CompilerCoursierTest / test := ((Scala3CompilerCoursierTest / test) dependsOn (
          publishLocal, // Had to enumarate all deps since calling `scala3-bootstrap` / publishLocal will lead to recursive dependency => stack overflow
          `scala3-interfaces` / publishLocal,
          dottyLibrary(Bootstrapped) / publishLocal,
          tastyCore(Bootstrapped) / publishLocal,
        ),
      ).value,
    )

  def dottyCompiler(implicit mode: Mode): Project = mode match {
    case NonBootstrapped => `scala3-compiler`
    case Bootstrapped => `scala3-compiler-bootstrapped`
  }

  // Settings shared between scala3-library, scala3-library-bootstrapped and scala3-library-bootstrappedJS
  def dottyLibrarySettings(implicit mode: Mode) = Seq(
    versionScheme := Some("semver-spec"),
    libraryDependencies += "org.scala-lang" % "scala-library" % stdlibVersion,
    (Compile / scalacOptions) ++= Seq(
      // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called
      // NOTE: Do not use `sourceDirectories` since `sources` are currently pinned until `3.8.0`
      "-sourcepath", (Compile / sources).value.map(_.getCanonicalPath).distinct.mkString(File.pathSeparator),
      "-Yexplicit-nulls",
    ),
    (Compile / doc / scalacOptions) ++= ScaladocConfigs.DefaultGenerationSettings.value.settings,
    (Compile / packageSrc / mappings) ++= {
      val auxBase = (ThisBuild / baseDirectory).value / "library-aux/src"
      auxBase ** "*.scala" pair io.Path.relativeTo(auxBase)
    },
  )

  lazy val `scala3-library` = project.in(file("library")).asDottyLibrary(NonBootstrapped)
    .settings(
      // Note: extracted using `print scala3-library / Compile / sources`
      // Only keep scala3 files until 3.8.0
      Compile / sources := Seq(
        file(s"${baseDirectory.value}/src/scala/Precise.scala"),
        file(s"${baseDirectory.value}/src/scala/CanEqual.scala"),
        file(s"${baseDirectory.value}/src/scala/Conversion.scala"),
        file(s"${baseDirectory.value}/src/scala/PolyFunction.scala"),
        file(s"${baseDirectory.value}/src/scala/IArray.scala"),
        file(s"${baseDirectory.value}/src/scala/CanThrow.scala"),
        file(s"${baseDirectory.value}/src/scala/Tuple.scala"),
        file(s"${baseDirectory.value}/src/scala/Selectable.scala"),
        file(s"${baseDirectory.value}/src/scala/main.scala"),
        file(s"${baseDirectory.value}/src/scala/NamedTuple.scala"),
        file(s"${baseDirectory.value}/src/scala/util/FromDigits.scala"),
        file(s"${baseDirectory.value}/src/scala/util/CommandLineParser.scala"),
        file(s"${baseDirectory.value}/src/scala/util/TupledFunction.scala"),
        file(s"${baseDirectory.value}/src/scala/util/NotGiven.scala"),
        file(s"${baseDirectory.value}/src/scala/util/boundary.scala"),
        file(s"${baseDirectory.value}/src/scala/caps/package.scala"),
        file(s"${baseDirectory.value}/src/scala/caps/Pure.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/TypeTest.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/Selectable.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/Typeable.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/Enum.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TupleMirror.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TypeBox.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/Arrays.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TupledFunctions.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/FunctionXXL.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/Scala3RunTime.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/$$throws.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/LazyVals.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/EnumValue.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TupleXXL.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/Tuples.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/MatchCase.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/retains.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/capability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/static.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/transparentTrait.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/RefiningAnnotation.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/retainsByName.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/threadUnsafe.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/constructorOnly.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/experimental.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/MacroAnnotation.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/alpha.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/publicInBinary.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/init.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/unroll.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/targetName.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/stableNull.scala"),
        file(s"${baseDirectory.value}/src/scala/deriving/Mirror.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/package.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Type.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Varargs.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Quotes.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Expr.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/ExprMap.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/FromExpr.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Exprs.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/ToExpr.scala"),
        file(s"${baseDirectory.value}/src/scala/util/control/NonLocalReturns.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/stdLibPatches/language.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/stdLibPatches/Predef.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure8.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure10.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure4.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure5.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure11.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure9.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure2.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure20.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure16.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure17.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure3.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure21.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure18.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure22.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure0.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure14.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure15.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure1.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure19.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure12.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure6.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure7.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure13.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/coverage/Invoker.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/ErasedParam.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/RuntimeChecked.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/CaptureChecked.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/ContextResultCount.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/TASTYSignature.java"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Alias.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/MappedAlternative.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Repeated.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/WithPureFuns.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Child.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/ProvisionalSuperClass.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/WitnessNames.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/AssignedNonLocally.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/preview.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/InlineParam.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/SourceFile.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/reachCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/$$into.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/TASTYLongSignature.java"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/readOnlyCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/unshared.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/AnnotationDefault.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/sharable.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Body.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/requiresCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/unchecked/uncheckedCaptures.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/unchecked/uncheckedCapabilityLeaks.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/testing/Error.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/testing/ErrorKind.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/testing/package.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/long.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/any.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/int.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/string.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/double.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/boolean.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/float.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/QuoteUnpickler.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/QuoteMatching.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/Expr.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/Patterns.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/SplicedType.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/StopMacroExpansion.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/Erased.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/onlyCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/VarArgsBuilder.scala"),
      )
    )
  lazy val `scala3-library-bootstrapped`: Project = project.in(file("library")).asDottyLibrary(Bootstrapped)
    .settings(
      // Note: extracted using `print scala3-library-bootstrapped / Compile / sources`
      // Only keep scala3 files until 3.8.0
      Compile / sources := Seq(
        file(s"${baseDirectory.value}/src/scala/Precise.scala"),
        file(s"${baseDirectory.value}/src/scala/CanEqual.scala"),
        file(s"${baseDirectory.value}/src/scala/Conversion.scala"),
        file(s"${baseDirectory.value}/src/scala/PolyFunction.scala"),
        file(s"${baseDirectory.value}/src/scala/IArray.scala"),
        file(s"${baseDirectory.value}/src/scala/CanThrow.scala"),
        file(s"${baseDirectory.value}/src/scala/Tuple.scala"),
        file(s"${baseDirectory.value}/src/scala/Selectable.scala"),
        file(s"${baseDirectory.value}/src/scala/main.scala"),
        file(s"${baseDirectory.value}/src/scala/NamedTuple.scala"),
        file(s"${baseDirectory.value}/src/scala/util/FromDigits.scala"),
        file(s"${baseDirectory.value}/src/scala/util/CommandLineParser.scala"),
        file(s"${baseDirectory.value}/src/scala/util/TupledFunction.scala"),
        file(s"${baseDirectory.value}/src/scala/util/NotGiven.scala"),
        file(s"${baseDirectory.value}/src/scala/util/boundary.scala"),
        file(s"${baseDirectory.value}/src/scala/caps/package.scala"),
        file(s"${baseDirectory.value}/src/scala/caps/Pure.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/TypeTest.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/Selectable.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/Typeable.scala"),
        file(s"${baseDirectory.value}/src/scala/reflect/Enum.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TupleMirror.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TypeBox.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/Arrays.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TupledFunctions.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/FunctionXXL.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/Scala3RunTime.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/$$throws.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/LazyVals.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/EnumValue.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/TupleXXL.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/Tuples.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/MatchCase.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/retains.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/capability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/static.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/transparentTrait.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/RefiningAnnotation.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/retainsByName.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/threadUnsafe.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/constructorOnly.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/experimental.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/MacroAnnotation.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/alpha.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/publicInBinary.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/init.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/unroll.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/targetName.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/stableNull.scala"),
        file(s"${baseDirectory.value}/src/scala/deriving/Mirror.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/package.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Type.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Varargs.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Quotes.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Expr.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/ExprMap.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/FromExpr.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/Exprs.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/ToExpr.scala"),
        file(s"${baseDirectory.value}/src/scala/util/control/NonLocalReturns.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/stdLibPatches/language.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/stdLibPatches/Predef.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure8.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure10.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure4.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure5.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure11.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure9.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure2.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure20.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure16.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure17.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure3.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure21.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure18.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure22.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure0.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure14.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure15.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure1.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure19.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure12.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure6.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure7.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/function/JProcedure13.java"),
        file(s"${baseDirectory.value}/src/scala/runtime/coverage/Invoker.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/ErasedParam.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/RuntimeChecked.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/CaptureChecked.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/ContextResultCount.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/TASTYSignature.java"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Alias.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/MappedAlternative.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Repeated.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/WithPureFuns.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Child.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/ProvisionalSuperClass.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/WitnessNames.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/AssignedNonLocally.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/preview.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/InlineParam.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/SourceFile.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/reachCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/$$into.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/TASTYLongSignature.java"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/readOnlyCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/unshared.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/AnnotationDefault.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/sharable.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/Body.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/requiresCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/unchecked/uncheckedCaptures.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/unchecked/uncheckedCapabilityLeaks.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/testing/Error.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/testing/ErrorKind.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/testing/package.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/long.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/any.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/int.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/string.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/double.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/boolean.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/ops/float.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/QuoteUnpickler.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/QuoteMatching.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/Expr.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/Patterns.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/SplicedType.scala"),
        file(s"${baseDirectory.value}/src/scala/quoted/runtime/StopMacroExpansion.scala"),
        file(s"${baseDirectory.value}/src/scala/compiletime/Erased.scala"),
        file(s"${baseDirectory.value}/src/scala/annotation/internal/onlyCapability.scala"),
        file(s"${baseDirectory.value}/src/scala/runtime/VarArgsBuilder.scala"),
      )
    )

  // ==============================================================================================
  // ================================= NON-BOOTSTRAPPED PROJECTS ==================================
  // ==============================================================================================

  lazy val `scala3-nonbootstrapped` = project
    .aggregate(`scala3-interfaces`, `scala3-library-nonbootstrapped` , `scala-library-nonbootstrapped`,
      `tasty-core-nonbootstrapped`, `scala3-compiler-nonbootstrapped`, `scala3-sbt-bridge-nonbootstrapped`)
    .settings(
      name          := "scala3-nonbootstrapped",
      moduleName    := "scala3-nonbootstrapped",
      version       := dottyNonBootstrappedVersion,
      // Nothing to be published by this project, it is only an aggregate
      Compile / publishArtifact := false,
      Test    / publishArtifact := false,
      // Nothing to be published by this project
      publish / skip := true,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-nonbootstrapped",
      scalac := Def.inputTaskDyn {
        val log = streams.value.log
        val externalDeps = (`scala3-compiler-nonbootstrapped` / Runtime / externalDependencyClasspath).value
        val stdlib = (`scala-library-nonbootstrapped` / Compile / packageBin).value.getAbsolutePath.toString()
        val dottyCompiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value.getAbsolutePath.toString()
        val args0: List[String] = spaceDelimited("<arg>").parsed.toList
        val decompile = args0.contains("-decompile")
        val printTasty = args0.contains("-print-tasty")
        val debugFromTasty = args0.contains("-Ythrough-tasty")
        val defaultOutputDirectory =
          if (printTasty || decompile || debugFromTasty || args0.contains("-d")) Nil
          else List("-d", ((ThisBuild / baseDirectory).value / "out" / "default-last-scalac-out.jar").getPath)
        val args = args0.filter(arg => arg != "-repl" && arg != "-decompile" &&
            arg != "-with-compiler" && arg != "-Ythrough-tasty" && arg != "-print-tasty")
        val main =
          if (decompile) "dotty.tools.dotc.decompiler.Main"
          else if (printTasty) "dotty.tools.dotc.core.tasty.TastyPrinter"
          else if (debugFromTasty) "dotty.tools.dotc.fromtasty.Debug"
          else "dotty.tools.dotc.Main"

        var extraClasspath = Seq(stdlib)

        if (decompile && !args.contains("-classpath"))
          extraClasspath ++= Seq(".")

        if (args0.contains("-with-compiler")) {
          log.error("-with-compiler should only be used with a bootstrapped compiler")
        }

        val wrappedArgs = if (printTasty) args else insertClasspathInArgs(args, extraClasspath.mkString(File.pathSeparator))
        val fullArgs = main :: (defaultOutputDirectory ::: wrappedArgs).map("\""+ _ + "\"").map(_.replace("\\", "\\\\"))

        (`scala3-compiler-nonbootstrapped` / Compile / runMain).toTask(fullArgs.mkString(" ", " ", ""))
      }.evaluated,
      testCompilation := Def.inputTaskDyn {
        val args = spaceDelimited("<arg>").parsed
        if (args.contains("--help")) {
          println(
            s"""
               |usage: testCompilation [--help] [--from-tasty] [--update-checkfiles] [--failed] [<filter>]
               |
               |By default runs tests in dotty.tools.dotc.*CompilationTests and dotty.tools.dotc.coverage.*,
               |excluding tests tagged with dotty.SlowTests.
               |
               |  --help                show this message
               |  --from-tasty          runs tests in dotty.tools.dotc.FromTastyTests
               |  --update-checkfiles   override the checkfiles that did not match with the current output
               |  --failed              re-run only failed tests
               |  <filter>              substring of the path of the tests file
               |
             """.stripMargin
          )
          (`scala3-compiler-nonbootstrapped` / Test / testOnly).toTask(" not.a.test")
        }
        else {
          val updateCheckfile = args.contains("--update-checkfiles")
          val rerunFailed = args.contains("--failed")
          val fromTasty = args.contains("--from-tasty")
          val args1 = if (updateCheckfile | fromTasty | rerunFailed) args.filter(x => x != "--update-checkfiles" && x != "--from-tasty" && x != "--failed") else args
          val test = if (fromTasty) "dotty.tools.dotc.FromTastyTests" else "dotty.tools.dotc.*CompilationTests dotty.tools.dotc.coverage.*"
          val cmd = s" $test -- --exclude-categories=dotty.SlowTests" +
            (if (updateCheckfile) " -Ddotty.tests.updateCheckfiles=TRUE" else "") +
            (if (rerunFailed) " -Ddotty.tests.rerunFailed=TRUE" else "") +
            (if (args1.nonEmpty) " -Ddotty.tests.filter=" + args1.mkString(" ") else "")
          (`scala3-compiler-nonbootstrapped` / Test / testOnly).toTask(cmd)
        }
      }.evaluated
    )

  /* Configuration of the org.scala-lang:scala3-sbt-bridge:*.**.**-nonbootstrapped project */
  lazy val `scala3-sbt-bridge-nonbootstrapped` = project.in(file("sbt-bridge"))
    .dependsOn(`scala3-compiler-nonbootstrapped`) // TODO: Would this actually evict the reference compiler in scala-tool?
    .settings(
      name          := "scala3-sbt-bridge-nonbootstrapped",
      moduleName    := "scala3-sbt-bridge",
      version       := dottyNonBootstrappedVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := false, // org.scala-lang:scala3-sbt-bridge doesn't have a crosspath
      autoScalaLibrary := false, // do not add a dependency to stdlib, we depend transitively on the stdlib from `scala3-compiler-nonbootstrapped`
      // Add the source directories for the sbt-bridge (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      Compile / resourceDirectory := baseDirectory.value / "resources",
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Add all the project's external dependencies
      libraryDependencies ++= Seq(
        ("org.scala-sbt" %% "zinc-apiinfo" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
        ),
      // Exclude the transitive dependencies from `zinc-apiinfo` that causes issues at the moment
      excludeDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect",
        "org.scala-lang" % "scala-compiler",
      ),
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // non-bootstrapped stdlib is publishable (only locally)
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-sbt-bridge-nonbootstrapped",
      // sbt adds all the projects to scala-tool config which breaks building the scalaInstance
      // as a workaround, I build it manually by only adding the compiler
      scalaInstance := {
        val lm = dependencyResolution.value
        val log = streams.value.log
        val retrieveDir = streams.value.cacheDirectory / "scala3-compiler" / scalaVersion.value
        val comp = lm.retrieve("org.scala-lang" % "scala3-compiler_3" %
          scalaVersion.value, scalaModuleInfo = None, retrieveDir, log)
          .fold(w => throw w.resolveException, identity)
        Defaults.makeScalaInstance(
          scalaVersion.value,
          Array.empty,
          comp.toSeq,
          Seq.empty,
          state.value,
          scalaInstanceTopLoader.value,
        )},
    )

  // ==============================================================================================
  // =================================== BOOTSTRAPPED PROJECTS ====================================
  // ==============================================================================================

  lazy val `scala3-bootstrapped-new` = project
    .aggregate(`scala3-interfaces`, `scala3-library-bootstrapped-new` , `scala-library-bootstrapped`,
      `tasty-core-bootstrapped-new`, `scala3-compiler-bootstrapped-new`, `scala3-sbt-bridge-bootstrapped`,
      `scala3-staging-new`, `scala3-tasty-inspector-new`, `scala-library-sjs`, `scala3-library-sjs`, `scaladoc-new`)
    .settings(
      name          := "scala3-bootstrapped",
      moduleName    := "scala3-bootstrapped",
      version       := dottyVersion,
      // Nothing to be published by this project, it is only an aggregate
      Compile / publishArtifact := false,
      Test    / publishArtifact := false,
      // Nothing to be published by this project
      publish / skip := true,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-bootstrapped",
      scalac := Def.inputTaskDyn {
        val log = streams.value.log
        val externalDeps = (`scala3-compiler-bootstrapped-new` / Runtime / externalDependencyClasspath).value
        val stdlib = (`scala-library-bootstrapped` / Compile / packageBin).value.getAbsolutePath.toString
        val dottyCompiler = (`scala3-compiler-bootstrapped-new` / Compile / packageBin).value.getAbsolutePath.toString
        val args0: List[String] = spaceDelimited("<arg>").parsed.toList
        val decompile = args0.contains("-decompile")
        val printTasty = args0.contains("-print-tasty")
        val debugFromTasty = args0.contains("-Ythrough-tasty")
        val defaultOutputDirectory =
          if (printTasty || decompile || debugFromTasty || args0.contains("-d")) Nil
          else List("-d", ((ThisBuild / baseDirectory).value / "out" / "default-last-scalac-out.jar").getPath)
        val args = args0.filter(arg => arg != "-repl" && arg != "-decompile" &&
            arg != "-with-compiler" && arg != "-Ythrough-tasty" && arg != "-print-tasty")
        val main =
          if (decompile) "dotty.tools.dotc.decompiler.Main"
          else if (printTasty) "dotty.tools.dotc.core.tasty.TastyPrinter"
          else if (debugFromTasty) "dotty.tools.dotc.fromtasty.Debug"
          else "dotty.tools.dotc.Main"

        var extraClasspath = Seq(stdlib)

        if (decompile && !args.contains("-classpath"))
          extraClasspath ++= Seq(".")

        if (args0.contains("-with-compiler")) {
          val dottyInterfaces = (`scala3-interfaces` / Compile / packageBin).value.getAbsolutePath.toString
          val dottyStaging = (`scala3-staging-new` / Compile / packageBin).value.getAbsolutePath.toString
          val dottyTastyInspector = (`scala3-tasty-inspector-new` / Compile / packageBin).value.getAbsolutePath.toString
          val tastyCore = (`tasty-core-bootstrapped` / Compile / packageBin).value.getAbsolutePath.toString
          val asm = findArtifactPath(externalDeps, "scala-asm")
          val compilerInterface = findArtifactPath(externalDeps, "compiler-interface")
          extraClasspath ++= Seq(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore, compilerInterface)
        }

        val wrappedArgs = if (printTasty) args else insertClasspathInArgs(args, extraClasspath.mkString(File.pathSeparator))
        val fullArgs = main :: (defaultOutputDirectory ::: wrappedArgs).map("\""+ _ + "\"").map(_.replace("\\", "\\\\"))

        (Compile / runMain).toTask(fullArgs.mkString(" ", " ", ""))
      }.evaluated,
      testCompilation := Def.inputTaskDyn {
        val args = spaceDelimited("<arg>").parsed
        if (args.contains("--help")) {
          println(
            s"""
               |usage: testCompilation [--help] [--from-tasty] [--update-checkfiles] [--failed] [<filter>]
               |
               |By default runs tests in dotty.tools.dotc.*CompilationTests and dotty.tools.dotc.coverage.*,
               |excluding tests tagged with dotty.SlowTests.
               |
               |  --help                show this message
               |  --from-tasty          runs tests in dotty.tools.dotc.FromTastyTests
               |  --update-checkfiles   override the checkfiles that did not match with the current output
               |  --failed              re-run only failed tests
               |  <filter>              substring of the path of the tests file
               |
             """.stripMargin
          )
          (`scala3-compiler-bootstrapped-new` / Test / testOnly).toTask(" not.a.test")
        }
        else {
          val updateCheckfile = args.contains("--update-checkfiles")
          val rerunFailed = args.contains("--failed")
          val fromTasty = args.contains("--from-tasty")
          val args1 = if (updateCheckfile | fromTasty | rerunFailed) args.filter(x => x != "--update-checkfiles" && x != "--from-tasty" && x != "--failed") else args
          val test = if (fromTasty) "dotty.tools.dotc.FromTastyTests" else "dotty.tools.dotc.*CompilationTests dotty.tools.dotc.coverage.*"
          val cmd = s" $test -- --exclude-categories=dotty.SlowTests" +
            (if (updateCheckfile) " -Ddotty.tests.updateCheckfiles=TRUE" else "") +
            (if (rerunFailed) " -Ddotty.tests.rerunFailed=TRUE" else "") +
            (if (args1.nonEmpty) " -Ddotty.tests.filter=" + args1.mkString(" ") else "")
          (`scala3-compiler-bootstrapped-new` / Test / testOnly).toTask(cmd)
        }
      }.evaluated
    )

  /* Configuration of the org.scala-lang:scala3-sbt-bridge:*.**.**-bootstrapped project */
  lazy val `scala3-sbt-bridge-bootstrapped` = project.in(file("sbt-bridge"))
    .dependsOn(`scala3-compiler-bootstrapped-new`) // TODO: Would this actually evict the reference compiler in scala-tool?
    .settings(publishSettings)
    .settings(
      name          := "scala3-sbt-bridge-bootstrapped",
      moduleName    := "scala3-sbt-bridge",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := false, // org.scala-lang:scala3-sbt-bridge doesn't have a crosspath
      autoScalaLibrary := false, // do not add a dependency to stdlib, we depend transitively on the stdlib from `scala3-compiler-nonbootstrapped`
      // Add the source directories for the sbt-bridge (boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      Compile / resourceDirectory := baseDirectory.value / "resources",
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Add all the project's external dependencies
      libraryDependencies ++= Seq(
        ("org.scala-sbt" %% "zinc-apiinfo" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
        ),
      // Packaging configuration of `scala3-sbt-bridge`
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // non-bootstrapped stdlib is publishable (only locally)
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-sbt-bridge-bootstrapped",
      // Configure to use the non-bootstrapped compiler
      managedScalaInstance := false,
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  /* Configuration of the org.scala-lang:scala3-staging:*.**.**-bootstrapped project */
  lazy val `scala3-staging-new` = project.in(file("staging"))
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-staging (see sbt-test/sbt-dotty/quoted-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    .dependsOn(`scala3-compiler-bootstrapped-new` % "provided; compile->runtime; test->test")
    .settings(publishSettings)
    .settings(
      name          := "scala3-staging",
      moduleName    := "scala3-staging",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion,
      crossPaths    := true, // org.scala-lang:scala3-staging has a crosspath
      autoScalaLibrary := false, // do not add a dependency to stdlib, we depend transitively on the stdlib from `scala3-compiler-bootstrapped`
      // Add the source directories for the sbt-bridge (boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Packaging configuration of `scala3-staging`
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      publish / skip := false,
      // Configure to use the non-bootstrapped compiler
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  /* Configuration of the org.scala-lang:scala3-tasty-inspector:*.**.**-bootstrapped project */
  lazy val `scala3-tasty-inspector-new` = project.in(file("tasty-inspector"))
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-tasty-inspector (see sbt-test/sbt-dotty/tasty-inspector-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    .dependsOn(`scala3-compiler-bootstrapped-new` % "provided; compile->runtime; test->test")
    .settings(publishSettings)
    .settings(
      name          := "scala3-tasty-inspector",
      moduleName    := "scala3-tasty-inspector",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion,
      crossPaths    := true, // org.scala-lang:scala3-tasty-inspector has a crosspath
      autoScalaLibrary := false, // do not add a dependency to stdlib, we depend transitively on the stdlib from `scala3-compiler-bootstrapped`
      // Add the source directories for the sbt-bridge (boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Packaging configuration of `scala3-staging`
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      publish / skip := false,
      // Configure to use the non-bootstrapped compiler
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  // ==============================================================================================
  // =================================== SCALA STANDARD LIBRARY ===================================
  // ==============================================================================================

  /* Configuration of the org.scala-lang:scala-library:*.**.**-nonbootstrapped project */
  lazy val `scala-library-nonbootstrapped` = project.in(file("library"))
    .enablePlugins(ScalaLibraryPlugin)
    .settings(
      name          := "scala-library-nonbootstrapped",
      moduleName    := "scala-library",
      version       := dottyNonBootstrappedVersion,
      // We mark the current library as "always" instead of "semver-spec" so that buildtools can
      // assume binary compatibility between 2.13.x and 3.x.y. If not set, build tools will, at least sbt,
      // will error by claiming that scala-library:2.13.x and 3.x.y are potentially binary incompatible.
      // Situation where we have 2.13.x and 3.x.y in the same dependency tree happens
      // because we allow cross-compilation.
      versionScheme := Some("always"),
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := false, // org.scala-lang:scala-library doesn't have a crosspath
      autoScalaLibrary := false, // do not add a dependency to stdlib
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src-non-bootstrapped",
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      Compile / scalacOptions += "-Yno-stdlib-patches",
      Compile / scalacOptions ++= Seq(
        // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called
        "-sourcepath", (Compile / sourceDirectories).value.map(_.getCanonicalPath).distinct.mkString(File.pathSeparator),
      ),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // non-bootstrapped stdlib is publishable (only locally)
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala-library-nonbootstrapped",
      // Add configuration for MiMa
      mimaCheckDirection := (compatMode match {
        case CompatMode.BinaryCompatible          => "backward"
        case CompatMode.SourceAndBinaryCompatible => "both"
      }),
      mimaExcludeAnnotations += "scala.annotation.experimental",
      mimaPreviousArtifacts += ("org.scala-lang" % "fat-stdlib" % "3.7.3"),
      mimaForwardIssueFilters := MiMaFilters.Scala3Library.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.Scala3Library.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.Scala3Library"),
    )

  /* Configuration of the org.scala-lang:scala3-library_3:*.**.**-nonbootstrapped project */
  lazy val `scala3-library-nonbootstrapped` = project.in(file("library"))
    .dependsOn(`scala-library-nonbootstrapped`)
    .settings(
      name          := "scala3-library-nonbootstrapped",
      moduleName    := "scala3-library",
      version       := dottyNonBootstrappedVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := true, // org.scala-lang:scala3-library has a crosspath
      // Do not depend on the `org.scala-lang:scala3-library` automatically, we manually depend on `scala-library-nonbootstrapped`
      autoScalaLibrary := false,
      // Drop all the scala tools in this project, so we can never generate any bytecode, or documentation
      managedScalaInstance := false,
      // This Project only has a dependency to `org.scala-lang:scala-library:*.**.**-nonbootstrapped`
      Compile / sources := Seq(),
      Compile / resources := Seq(),
      Test / sources := Seq(),
      Test / resources := Seq(),
      // Bridge the common task to call the ones of the actual library project
      Compile / compile := (`scala-library-nonbootstrapped` / Compile / compile).value,
      Compile / doc     := (`scala-library-nonbootstrapped` / Compile / doc).value,
      Compile / run     := (`scala-library-nonbootstrapped` / Compile / run).evaluated,
      Test / compile := (`scala-library-nonbootstrapped` / Test / compile).value,
      Test / doc     := (`scala-library-nonbootstrapped` / Test / doc).value,
      Test / run     := (`scala-library-nonbootstrapped` / Test / run).evaluated,
      Test / test    := (`scala-library-nonbootstrapped` / Test / test).value,
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-library-nonbootstrapped",
    )

  /* Configuration of the org.scala-lang:scala-library:*.**.**-bootstrapped project */
  lazy val `scala-library-bootstrapped` = project.in(file("library"))
    .enablePlugins(ScalaLibraryPlugin)
    .settings(publishSettings)
    .settings(
      name          := "scala-library-bootstrapped",
      moduleName    := "scala-library",
      version       := dottyVersion,
      // We mark the current library as "always" instead of "semver-spec" so that buildtools can
      // assume binary compatibility between 2.13.x and 3.x.y. If not set, build tools will, at least sbt,
      // will error by claiming that scala-library:2.13.x and 3.x.y are potentially binary incompatible.
      // Situation where we have 2.13.x and 3.x.y in the same dependency tree happens
      // because we allow cross-compilation.
      versionScheme := Some("always"),
      // sbt defaults to scala 2.12.x and metals will report issues as it doesn't consider the project a scala 3 project
      // (not the actual version we use to compile the project)
      scalaVersion  := referenceVersion,
      crossPaths    := false, // org.scala-lang:scala-library doesn't have a crosspath
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src-bootstrapped",
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions :=  Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      Compile / scalacOptions += "-Yno-stdlib-patches",
      Compile / scalacOptions ++= Seq(
        // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called
        "-sourcepath", (Compile / sourceDirectories).value.map(_.getCanonicalPath).distinct.mkString(File.pathSeparator),
      ),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala-library-bootstrapped",
      // we need to have the `scala-library` artifact in the classpath for `ScalaLibraryPlugin` to work
      // this was the only way to not get the artifact evicted by sbt. Even a custom configuration didn't work
      // NOTE: true is the default value, just making things clearer here
      managedScalaInstance := true,
      // Configure the nonbootstrapped compiler
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      // Add configuration for MiMa
      mimaCheckDirection := (compatMode match {
        case CompatMode.BinaryCompatible          => "backward"
        case CompatMode.SourceAndBinaryCompatible => "both"
      }),
      mimaExcludeAnnotations += "scala.annotation.experimental",
      mimaPreviousArtifacts += ("org.scala-lang" % "fat-stdlib" % "3.7.3"),
      mimaForwardIssueFilters := MiMaFilters.Scala3Library.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.Scala3Library.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.Scala3Library"),
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  /* Configuration of the org.scala-lang:scala3-library_3:*.**.**-bootstrapped project */
  lazy val `scala3-library-bootstrapped-new` = project.in(file("library"))
    .dependsOn(`scala-library-bootstrapped`)
    .settings(publishSettings)
    .settings(
      name          := "scala3-library-bootstrapped",
      moduleName    := "scala3-library",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      // sbt defaults to scala 2.12.x and metals will report issues as it doesn't consider the project a scala 3 project
      // (not the actual version we use to compile the project)
      scalaVersion  := referenceVersion,
      crossPaths    := true, // org.scala-lang:scala3-library has a crosspath
      // Do not depend on the `org.scala-lang:scala3-library` automatically, we manually depend on `scala-library-bootstrapped`
      autoScalaLibrary := false,
      // Drop all the scala tools in this project, so we can never generate any bytecode, or documentation
      managedScalaInstance := false,
      // This Project only has a dependency to `org.scala-lang:scala-library:*.**.**-bootstrapped`
      Compile / sources := Seq(),
      Compile / resources := Seq(),
      Test / sources := Seq(),
      Test / resources := Seq(),
      // Bridge the common task to call the ones of the actual library project
      Compile / compile := (`scala-library-bootstrapped` / Compile / compile).value,
      Compile / doc     := (`scala-library-bootstrapped` / Compile / doc).value,
      Compile / run     := (`scala-library-bootstrapped` / Compile / run).evaluated,
      Test / compile := (`scala-library-bootstrapped` / Test / compile).value,
      Test / doc     := (`scala-library-bootstrapped` / Test / doc).value,
      Test / run     := (`scala-library-bootstrapped` / Test / run).evaluated,
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-library-bootstrapped",
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  /* Configuration of the org.scala-js:scalajs-scalalib_2.13:*.**.**-bootstrapped project */
  lazy val `scala-library-sjs` = project.in(file("library-js"))
    // We add a dependency to the JVM library to have the classfile available
    // (as they are not part of this artifact)
    .dependsOn(`scala3-library-bootstrapped-new`)
    .settings(publishSettings)
    .settings(
      name          := "scala-library-sjs",
      organization  := "org.scala-js",
      // This is very tricky here since this is a Scala 3 project, but to be able to smoothly
      // migrate the ecosystem, we need to be able to evict the Scala 2 library from the classpath.
      // The problem is that the Scala 2 library for Scala.js has a _2.13 in the module's name, so we need
      // to release Scala 3 for Scala.js with the same _2.13 instead of the _3.
      // Yes, I know, this is weird and feels wrong.
      moduleName    := "scalajs-scalalib_2.13",
      version       := dottyVersion,
      // We mark the current library as "always" instead of "semver-spec" so that buildtools can
      // assume binary compatibility between 2.13.x and 3.x.y. If not set, build tools will, at least sbt,
      // will error by claiming that scalajs-scalalib_2.13:2.13.x and 3.x.y are potentially binary incompatible.
      // Situation where we have 2.13.x and 3.x.y in the same dependency tree happens
      // because we allow cross-compilation.
      versionScheme := Some("always"),
      crossPaths    := false,
      // sbt defaults to scala 2.12.x and metals will report issues as it doesn't consider the project a scala 3 project
      // (not the actual version we use to compile the project)
      scalaVersion  := referenceVersion,
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Compile / unmanagedSourceDirectories ++=
        (`scala-library-bootstrapped` / Compile / unmanagedSourceDirectories).value,
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions :=  Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions", "-nowarn"),
      Compile / scalacOptions += "-Yno-stdlib-patches",
      Compile / scalacOptions += "-scalajs",
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Take into account the source files from the `library` folder
      // but give the priority to the files in `library-js` that override files in `library`
      Compile / sources := {
        val files = (Compile / sources).value
        val overwrittenSources =
          (files ++ Seq(
              baseDirectory.value / "src" / "scala" / "runtime" / "BoxesRunTime.java",
              baseDirectory.value / "src" / "scala" / "math" / "ScalaNumber.java",
          ))
          .flatMap(_.relativeTo(baseDirectory.value / "src")).toSet

        files.filterNot(file =>
          file.relativeTo((`scala-library-bootstrapped` / baseDirectory).value / "src")
            .exists(overwrittenSources.contains))

      },
      // Drop all the tasty files and the classfiles when packaging bu the scalajs exclusive classes
      // More info here: https://github.com/scala-js/scala-js/issues/5217
      Compile / packageBin / mappings := {
        (Compile / packageBin / mappings).value.filter(file =>
          file._2.endsWith(".sjsir")
          || file._2.endsWith("UnitOps.tasty")         || file._2.endsWith("UnitOps.class") || file._2.endsWith("UnitOps$.class")
          || file._2.endsWith("AnonFunctionXXL.tasty") || file._2.endsWith("AnonFunctionXXL.class"))
      },
      libraryDependencies += ("org.scala-js" %% "scalajs-library" % scalaJSVersion % Provided).cross(CrossVersion.for3Use2_13),
      libraryDependencies += ("org.scala-js" % "scalajs-javalib" % scalaJSVersion),
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala-library",
      // we need to have the `scala-library` artifact in the classpath for `ScalaLibraryPlugin` to work
      // this was the only way to not get the artifact evicted by sbt. Even a custom configuration didn't work
      // NOTE: true is the default value, just making things clearer here
      managedScalaInstance := true,
      autoScalaLibrary := false,
      // Configure the nonbootstrapped compiler
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      // See https://stackoverflow.com/a/51416386
      pomPostProcess := { (node: XmlNode) =>
        new RuleTransformer(new RewriteRule {
          override def transform(node: XmlNode): XmlNodeSeq = node match {
            case e: Elem if e.label == "dependency" && e.child.exists(child => child.label == "artifactId" && child.text == "scalajs-library_2.13") =>
              XmlNodeSeq.Empty
            case _ => node
        }
      }).transform(node).head
      },
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  /* Configuration of the org.scala-lang:scala3-library_sjs1_3:*.**.**-bootstrapped project */
  lazy val `scala3-library-sjs` = project.in(file("library-js"))
    .dependsOn(`scala-library-sjs`)
    .settings(publishSettings)
    .settings(
      name          := "scala3-library-sjs",
      moduleName    := "scala3-library_sjs1",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      // sbt defaults to scala 2.12.x and metals will report issues as it doesn't consider the project a scala 3 project
      // (not the actual version we use to compile the project)
      scalaVersion  := referenceVersion,
      crossPaths    := true, // org.scala-lang:scala3-library_sjs1 has a crosspath
      // Do not depend on the `org.scala-lang:scala3-library` automatically, we manually depend on `scala-library-bootstrapped`
      autoScalaLibrary := false,
      // Drop all the scala tools in this project, so we can never generate any bytecode, or documentation
      managedScalaInstance := false,
      // This Project only has a dependency to `org.scala-js:scalajs-scalalib:*.**.**-bootstrapped`
      Compile / sources := Seq(),
      Compile / resources := Seq(),
      Test / sources := Seq(),
      Test / resources := Seq(),
      // Bridge the common task to call the ones of the actual library project
      Compile / compile := (`scala-library-sjs` / Compile / compile).value,
      Compile / doc     := (`scala-library-sjs` / Compile / doc).value,
      Compile / run     := (`scala-library-sjs` / Compile / run).evaluated,
      Test / compile := (`scala-library-sjs` / Test / compile).value,
      Test / doc     := (`scala-library-sjs` / Test / doc).value,
      Test / run     := (`scala-library-sjs` / Test / run).evaluated,
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-library",
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  // ==============================================================================================
  // ===================================== TASTY CORE LIBRARY =====================================
  // ==============================================================================================

  /* Configuration of the org.scala-lang:tasty-core_3:*.**.**-nonbootstrapped project */
  lazy val `tasty-core-nonbootstrapped` = project.in(file("tasty"))
    .dependsOn(`scala3-library-nonbootstrapped`)
    .settings(commonMiMaSettings)
    .settings(
      name          := "tasty-core-nonbootstrapped",
      moduleName    := "tasty-core",
      version       := dottyNonBootstrappedVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := true, // org.scala-lang:tasty-core has a crosspath
      // sbt shouldn't add stdlib automatically, we depend on `scala3-library-nonbootstrapped`
      autoScalaLibrary := false,
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src-non-bootstrapped",
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Add all the project's external dependencies
      libraryDependencies ++= Seq(
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      ),
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "tasty-core-nonbootstrapped",
      // sbt adds all the projects to scala-tool config which breaks building the scalaInstance
      // as a workaround, I build it manually by only adding the compiler
      scalaInstance := {
        val lm = dependencyResolution.value
        val log = streams.value.log
        val retrieveDir = streams.value.cacheDirectory / "scala3-compiler" / scalaVersion.value
        val comp = lm.retrieve("org.scala-lang" % "scala3-compiler_3" %
          scalaVersion.value, scalaModuleInfo = None, retrieveDir, log)
          .fold(w => throw w.resolveException, identity)
        Defaults.makeScalaInstance(
          scalaVersion.value,
          Array.empty,
          comp.toSeq,
          Seq.empty,
          state.value,
          scalaInstanceTopLoader.value,
        )},
      // Add configuration of the test
      Test / envVars ++= Map(
        "EXPECTED_TASTY_VERSION" -> expectedTastyVersion,
      ),
      mimaForwardIssueFilters := MiMaFilters.TastyCore.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.TastyCore.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.TastyCore"),
    )

  /* Configuration of the org.scala-lang:tasty-core_3:*.**.**-bootstrapped project */
  lazy val `tasty-core-bootstrapped-new` = project.in(file("tasty"))
    .dependsOn(`scala3-library-bootstrapped-new`)
    .settings(publishSettings)
    .settings(commonMiMaSettings)
    .settings(
      name          := "tasty-core-bootstrapped",
      moduleName    := "tasty-core",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := true, // org.scala-lang:tasty-core has a crosspath
      // sbt shouldn't add stdlib automatically, we depend on `scala3-library-nonbootstrapped`
      autoScalaLibrary := false,
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src-bootstrapped",
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Add all the project's external dependencies
      libraryDependencies ++= Seq(
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      ),
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "tasty-core-bootstrapped",
      // Configure to use the non-bootstrapped compiler
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      // Add configuration of the test
      Test / envVars ++= Map(
        "EXPECTED_TASTY_VERSION" -> expectedTastyVersion,
      ),
      mimaForwardIssueFilters := MiMaFilters.TastyCore.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.TastyCore.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.TastyCore"),
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  // ==============================================================================================
  // ======================================= SCALA COMPILER =======================================
  // ==============================================================================================

  /* Configuration of the org.scala-lang:scala3-compiler_3:*.**.**-nonbootstrapped project */
  lazy val `scala3-compiler-nonbootstrapped` = project.in(file("compiler"))
    .dependsOn(`scala3-interfaces`, `tasty-core-nonbootstrapped`, `scala3-library-nonbootstrapped`)
    .settings(
      name          := "scala3-compiler-nonbootstrapped",
      moduleName    := "scala3-compiler",
      version       := dottyNonBootstrappedVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := true, // org.scala-lang:scala3-compiler has a crosspath
      // sbt shouldn't add stdlib automatically, we depend on `scala3-library-nonbootstrapped`
      autoScalaLibrary := false,
      // Add the source directories for the compiler (non-boostrapped)
      Compile / unmanagedSourceDirectories   := Seq(baseDirectory.value / "src"),
      Compile / unmanagedSourceDirectories   += baseDirectory.value / "src-non-bootstrapped",
      Compile / unmanagedResourceDirectories += baseDirectory.value / "resources",
      // Add the test directories for the compiler (non-bootstrapped)
      Test / unmanagedSourceDirectories   := Seq(baseDirectory.value / "test"),
      Test / unmanagedResourceDirectories += baseDirectory.value / "test-resources",
      // All the dependencies needed by the compiler
      libraryDependencies ++= Seq(
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
        "org.scala-lang.modules" % "scala-asm" % "9.8.0-scala-1",
        Dependencies.compilerInterface,
        "org.jline" % "jline-reader" % "3.29.0",
        "org.jline" % "jline-terminal" % "3.29.0",
        "org.jline" % "jline-terminal-jni" % "3.29.0",
        ("io.get-coursier" %% "coursier" % "2.0.16" % Test).cross(CrossVersion.for3Use2_13),
      ),
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // TODO: Enable these flags when the new stdlib is explicitelly null checked
      //Compile / scalacOptions ++= Seq("-Yexplicit-nulls", "-Wsafe-init"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Specify the default entry point of the compiler
      Compile / mainClass := Some("dotty.tools.dotc.Main"),
      // Add entry's to the MANIFEST
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)), // Used by the REPL
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-compiler-nonbootstrapped",
      // Generate compiler.properties, used by sbt
      Compile / resourceGenerators += Def.task {
        import java.util._
        import java.text._
        val file = (Compile / resourceManaged).value / "compiler.properties"
        val dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss")
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"))
        val contents =                //2.11.11.v20170413-090219-8a413ba7cc
          s"""version.number=${version.value}
             |maven.version.number=${version.value}
             |git.hash=${VersionUtil.gitHash}
             |copyright.string=Copyright 2002-$currentYear, LAMP/EPFL
           """.stripMargin

        if (!(file.exists && IO.read(file) == contents)) {
          IO.write(file, contents)
        }

        Seq(file)
      }.taskValue,
      (Compile / sourceGenerators) += ShadedSourceGenerator.task.taskValue,
      // sbt adds all the projects to scala-tool config which breaks building the scalaInstance
      // as a workaround, I build it manually by only adding the compiler
      managedScalaInstance := false,
      scalaInstance := {
        val lm = dependencyResolution.value
        val log = streams.value.log
        val retrieveDir = streams.value.cacheDirectory / "scala3-compiler" / referenceVersion
        val comp = lm.retrieve("org.scala-lang" % "scala3-compiler_3" %
          referenceVersion, scalaModuleInfo = None, retrieveDir, log)
          .fold(w => throw w.resolveException, identity)
        Defaults.makeScalaInstance(
          referenceVersion,
          Array.empty,
          comp.toSeq,
          Seq.empty,
          state.value,
          scalaInstanceTopLoader.value,
        )},
      scalaCompilerBridgeBinaryJar := {
        val lm = dependencyResolution.value
        val log = streams.value.log
        val retrieveDir = streams.value.cacheDirectory / "scala3-sbt-bridge" / referenceVersion
        val comp = lm.retrieve("org.scala-lang" % "scala3-sbt-bridge" %
          referenceVersion, scalaModuleInfo = None, retrieveDir, log)
          .fold(w => throw w.resolveException, identity)
        Some(comp(0))
      },
      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps").cross(CrossVersion.for3Use2_13),
      Compile / sourceGenerators += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (Compile / sourceManaged).value / "scalajs-ir-src"

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

          val sjsSources = (trgDir ** "*.scala").get.toSet
          sjsSources.foreach(f => {
            val lines = IO.readLines(f)
            val linesWithPackage = replacePackage(lines) {
              case "org.scalajs.ir" => "dotty.tools.sjs.ir"
            }
            IO.writeLines(f, insertUnsafeNullsImport(linesWithPackage))
          })
          sjsSources
        } (Set(scalaJSIRSourcesJar)).toSeq
      }.taskValue,
      // Configuration of the test suite
      Compile / run / forkOptions := (Compile / run / forkOptions).value
        .withWorkingDirectory((ThisBuild / baseDirectory).value),
      Test / forkOptions := (Test / forkOptions).value
        .withWorkingDirectory((ThisBuild / baseDirectory).value),
      Test / test := (Test / testOnly).toTask(" -- --exclude-categories=dotty.VulpixMetaTests").value,
      Test / testOptions += Tests.Argument(
        TestFrameworks.JUnit,
        "--run-listener=dotty.tools.ContextEscapeDetector", "--exclude-categories=dotty.BootstrappedOnlyTests",
      ),
      Test / javaOptions ++= {
        val log = streams.value.log
        val managedSrcDir = {
          // Populate the directory
          (Compile / managedSources).value

          (Compile / sourceManaged).value
        }
        val externalDeps = (ThisProject / Runtime / externalDependencyClasspath).value
        Seq(
          s"-Ddotty.tests.dottyCompilerManagedSources=${managedSrcDir}",
          s"-Ddotty.tests.classes.dottyInterfaces=${(`scala3-interfaces` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.dottyCompiler=${(ThisProject / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.tastyCore=${(`tasty-core-nonbootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.compilerInterface=${findArtifactPath(externalDeps, "compiler-interface")}",
          s"-Ddotty.tests.classes.scalaLibrary=${(`scala-library-nonbootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.scalaAsm=${findArtifactPath(externalDeps, "scala-asm")}",
          s"-Ddotty.tests.classes.jlineTerminal=${findArtifactPath(externalDeps, "jline-terminal")}",
          s"-Ddotty.tests.classes.jlineReader=${findArtifactPath(externalDeps, "jline-reader")}",
          s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
        )
      },
    )

  /* Configuration of the org.scala-lang:scala3-compiler_3:*.**.**-bootstrapped project */
  lazy val `scala3-compiler-bootstrapped-new` = project.in(file("compiler"))
    .dependsOn(`scala3-interfaces`, `tasty-core-bootstrapped-new`, `scala3-library-bootstrapped-new`)
    .settings(publishSettings)
    .settings(
      name          := "scala3-compiler-bootstrapped",
      moduleName    := "scala3-compiler",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := true, // org.scala-lang:scala3-compiler has a crosspath
      // sbt shouldn't add stdlib automatically, we depend on `scala3-library-nonbootstrapped`
      autoScalaLibrary := false,
      // Add the source directories for the compiler (boostrapped)
      Compile / unmanagedSourceDirectories   := Seq(baseDirectory.value / "src"),
      Compile / unmanagedSourceDirectories   += baseDirectory.value / "src-bootstrapped",
      Compile / unmanagedResourceDirectories += baseDirectory.value / "resources",
      // Add the test directories for the compiler (bootstrapped)
      Test / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      Test / unmanagedResourceDirectories += baseDirectory.value / "test-resources",
      // All the dependencies needed by the compiler
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" % "scala-asm" % "9.8.0-scala-1",
        Dependencies.compilerInterface,
        "org.jline" % "jline-reader" % "3.29.0",
        "org.jline" % "jline-terminal" % "3.29.0",
        "org.jline" % "jline-terminal-jni" % "3.29.0",
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
        ("io.get-coursier" %% "coursier" % "2.0.16" % Test).cross(CrossVersion.for3Use2_13),
      ),
      // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      // TODO: Enable these flags when the new stdlib is explicitelly null checked
      //Compile / scalacOptions ++= Seq("-Yexplicit-nulls", "-Wsafe-init"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Specify the default entry point of the compiler
      Compile / mainClass := Some("dotty.tools.dotc.Main"),
      // Add entry's to the MANIFEST
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)), // Used by the REPL
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-compiler-bootstrapped",
      // Generate compiler.properties, used by sbt
      Compile / resourceGenerators += Def.task {
        import java.util._
        import java.text._
        val file = (Compile / resourceManaged).value / "compiler.properties"
        val dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss")
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"))
        val contents =                //2.11.11.v20170413-090219-8a413ba7cc
          s"""version.number=${version.value}
             |maven.version.number=${version.value}
             |git.hash=${VersionUtil.gitHash}
             |copyright.string=Copyright 2002-$currentYear, LAMP/EPFL
           """.stripMargin

        if (!(file.exists && IO.read(file) == contents)) {
          IO.write(file, contents)
        }

        Seq(file)
      }.taskValue,
      // Configure to use the non-bootstrapped compiler
      managedScalaInstance := false,
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      /* Add the sources of scalajs-ir.
       * To guarantee that dotty can bootstrap without depending on a version
       * of scalajs-ir built with a different Scala compiler, we add its
       * sources instead of depending on the binaries.
       */
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-ir" % scalaJSVersion % "sourcedeps").cross(CrossVersion.for3Use2_13),
      Compile / sourceGenerators += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val trgDir = (Compile / sourceManaged).value / "scalajs-ir-src"

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

          val sjsSources = (trgDir ** "*.scala").get.toSet
          sjsSources.foreach(f => {
            val lines = IO.readLines(f)
            val linesWithPackage = replacePackage(lines) {
              case "org.scalajs.ir" => "dotty.tools.sjs.ir"
            }
            IO.writeLines(f, insertUnsafeNullsImport(linesWithPackage))
          })
          sjsSources
        } (Set(scalaJSIRSourcesJar)).toSeq
      }.taskValue,
      (Compile / sourceGenerators) += ShadedSourceGenerator.task.taskValue,
      Compile / run / forkOptions := (Compile / run / forkOptions).value
        .withWorkingDirectory((ThisBuild / baseDirectory).value),
      // Configuration of the test suite
      Test / forkOptions := (Test / forkOptions).value
        .withWorkingDirectory((ThisBuild / baseDirectory).value),
      Test / test := (Test / testOnly).toTask(" -- --exclude-categories=dotty.VulpixMetaTests").value,
      Test / testOptions += Tests.Argument(
        TestFrameworks.JUnit,
        "--run-listener=dotty.tools.ContextEscapeDetector",
      ),
      Test / javaOptions ++= {
        val log = streams.value.log
        val managedSrcDir = {
          // Populate the directory
          (Compile / managedSources).value

          (Compile / sourceManaged).value
        }
        val externalDeps = (ThisProject / Runtime / externalDependencyClasspath).value
        Seq(
          s"-Ddotty.tests.dottyCompilerManagedSources=${managedSrcDir}",
          s"-Ddotty.tests.classes.dottyInterfaces=${(`scala3-interfaces` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.dottyCompiler=${(ThisProject / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.tastyCore=${(`tasty-core-bootstrapped-new` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.compilerInterface=${findArtifactPath(externalDeps, "compiler-interface")}",
          s"-Ddotty.tests.classes.scalaLibrary=${(`scala-library-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.scalaAsm=${findArtifactPath(externalDeps, "scala-asm")}",
          s"-Ddotty.tests.classes.jlineTerminal=${findArtifactPath(externalDeps, "jline-terminal")}",
          s"-Ddotty.tests.classes.jlineReader=${findArtifactPath(externalDeps, "jline-reader")}",
          s"-Ddotty.tests.classes.dottyStaging=${(LocalProject("scala3-staging-new") / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.dottyTastyInspector=${(LocalProject("scala3-tasty-inspector-new") / Compile / packageBin).value}",
          s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
        )
      },
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

  // ==============================================================================================
  // ========================================== SCALADOC ==========================================
  // ==============================================================================================

  /* Configuration of the org.scala-lang:scaladoc_3:*.**.**-bootstrapped project */
  lazy val `scaladoc-new` = project.in(file("scaladoc"))
    .dependsOn(`scala3-compiler-bootstrapped-new`, `scala3-tasty-inspector-new`)
    .settings(publishSettings)
    .settings(
      name          := "scaladoc",
      moduleName    := "scaladoc",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
      crossPaths    := true, // org.scala-lang:scaladoc has a crosspath
      // sbt shouldn't add stdlib automatically, we depend on `scala3-library-nonbootstrapped`
      autoScalaLibrary := false,
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Compile / resourceDirectory := baseDirectory.value / "resources",
      // Add all the necessary resource generators
      Compile / resourceGenerators ++= Seq(
        generateStaticAssetsTask.taskValue,
        bundleCSS.taskValue
      ),
      // All the dependencies needed by the doctool
      libraryDependencies ++= Dependencies.flexmarkDeps ++ Seq(
        "nl.big-o" % "liqp" % "0.8.2",
        "org.jsoup" % "jsoup" % "1.17.2", // Needed to process .html files for static site
        Dependencies.`jackson-dataformat-yaml`,
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      ),
       // NOTE: The only difference here is that we drop `-Werror` and semanticDB for now
      Compile / scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-encoding", "UTF8", "-language:implicitConversions"),
      Compile / scalacOptions += "-experimental",
      // TODO: Enable these flags when the new stdlib is explicitelly null checked
      //Compile / scalacOptions ++= Seq("-Yexplicit-nulls", "-Wsafe-init"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      Compile / javacOptions  ++= Seq("--release", Versions.minimumJVMVersion),
      Compile / scalacOptions ++= Seq("--java-output-version", Versions.minimumJVMVersion),
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := false,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      //
      Compile / mainClass := Some("dotty.tools.scaladoc.Main"),
      Compile / buildInfoKeys := Seq[BuildInfoKey](version),
      Compile / buildInfoPackage := "dotty.tools.scaladoc",
      BuildInfoPlugin.buildInfoScopedSettings(Compile),
      BuildInfoPlugin.buildInfoDefaultSettings,
      // Configure to use the non-bootstrapped compiler
      scalaInstance := {
        val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

        // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
        // just directories containing classfiles because sbt maintains a cache of
        // compiler instances. This cache is invalidated based on timestamps
        // however this is only implemented on jars, directories are never
        // invalidated.
        val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
        val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
        val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
        val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

        Defaults.makeScalaInstance(
          dottyNonBootstrappedVersion,
          libraryJars     = Array(scalaLibrary),
          allCompilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps,
          allDocJars      = Seq.empty,
          state.value,
          scalaInstanceTopLoader.value
        )
      },
      scalaCompilerBridgeBinaryJar := {
        Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
      },
      // Force recomplilation when bootstrapped compiler changes
      Compile / extraDevelocityCacheInputFiles ++=
        (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
    )

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
    enablePlugins(DottyJSPlugin).
    settings(
      commonBootstrappedSettings,
      libraryDependencies +=
        ("org.scala-js" %% "scalajs-library" % scalaJSVersion).cross(CrossVersion.for3Use2_13),
      // NOTE: Until 3.8.0, we pin the source files to be used by the scala3 library
      Compile / sources := (`scala3-library-bootstrapped` / Compile / sources).value,
      Compile / sources ++= Seq(
        file(s"${baseDirectory.value}/src/scala/scalajs/js/internal/UnitOps.scala"),
        file(s"${baseDirectory.value}/src/scala/scalajs/runtime/AnonFunctionXXL.scala"),
      ),
      // NOTE: We keep this so that the mappings are correct when packaging
      Compile / unmanagedSourceDirectories ++=
        (`scala3-library-bootstrapped` / Compile / unmanagedSourceDirectories).value,

      // Configure the source maps to point to GitHub for releases
      scalacOptions ++= {
        if (isRelease) {
          val baseURI = (LocalRootProject / baseDirectory).value.toURI
          val dottyVersion = version.value
          Seq(s"-scalajs-mapSourceURI:$baseURI->$dottyGithubRawUserContentUrl/$dottyVersion/")
        } else {
          Nil
        }
      },

      // Make sure `scala3-bootstrapped/test` doesn't fail on this project for no reason
      Test / test := {},
      Test / testOnly := {},
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
    // when compiling a project that depends on scala3-staging (see sbt-test/sbt-dotty/quoted-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value
    )

  lazy val `scala3-tasty-inspector` = project.in(file("tasty-inspector")).
    withCommonSettings(Bootstrapped).
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-tasty-inspector (see sbt-test/sbt-dotty/tasty-inspector-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    dependsOn(dottyCompiler(Bootstrapped) % "provided; compile->runtime; test->test").
    settings(
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value
    )

  lazy val `scala3-sbt-bridge` = project.in(file("sbt-bridge/src")).
    // We cannot depend on any bootstrapped project to compile the bridge, since the
    // bridge is needed to compile these projects.
    dependsOn(`scala3-compiler` % Provided).
    settings(commonJavaSettings).
    settings(
      description := "sbt compiler bridge for Dotty",

      Test / sources := Seq(),
      Compile / scalaSource := baseDirectory.value,
      Compile / javaSource := baseDirectory.value,
      Compile / resourceDirectory := baseDirectory.value.getParentFile / "resources",

      // Referring to the other project using a string avoids an infinite loop
      // when sbt reads the settings.
      Test / test := (LocalProject("scala3-sbt-bridge-tests") / Test / test).value,

      libraryDependencies += Dependencies.compilerInterface % Provided
    )

  // We use a separate project for the bridge tests since they can only be run
  // with the bootstrapped library on the classpath.
  lazy val `scala3-sbt-bridge-tests` = project.in(file("sbt-bridge/test")).
    dependsOn(dottyCompiler(Bootstrapped) % Test).
    dependsOn(`scala3-sbt-bridge`).
    settings(commonBootstrappedSettings).
    settings(
      Compile / sources := Seq(),
      Test / scalaSource := baseDirectory.value,
      Test / javaSource := baseDirectory.value,
      libraryDependencies += ("org.scala-sbt" %% "zinc-apiinfo" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13)
    )

  lazy val `scala3-presentation-compiler` = project.in(file("presentation-compiler"))
    .withCommonSettings(Bootstrapped)
    .dependsOn(`scala3-compiler-bootstrapped`, `scala3-library-bootstrapped`, `scala3-presentation-compiler-testcases` % "test->test")
    .settings(presentationCompilerSettings)
    .settings(scala3PresentationCompilerBuildInfo)

  def scala3PresentationCompilerBuildInfo =
    Seq(
      ideTestsDependencyClasspath := {
        val testCasesLib = (`scala3-presentation-compiler-testcases` / Compile / classDirectory).value
        val dottyLib = (`scala3-library-bootstrapped` / Compile / classDirectory).value
        val scalaLib =
          (`scala3-library-bootstrapped` / Compile / dependencyClasspath)
            .value
            .map(_.data)
            .filter(_.getName.matches("scala-library.*\\.jar"))
            .toList
        testCasesLib :: dottyLib :: scalaLib
        // Nil
      },
      Compile / buildInfoPackage := "dotty.tools.pc.buildinfo",
      Compile / buildInfoKeys := Seq(scalaVersion),
      Test / buildInfoPackage := "dotty.tools.pc.tests.buildinfo",
      Test / buildInfoKeys := Seq(scalaVersion, ideTestsDependencyClasspath)
    ) ++ BuildInfoPlugin.buildInfoScopedSettings(Compile) ++
      BuildInfoPlugin.buildInfoScopedSettings(Test) ++
      BuildInfoPlugin.buildInfoDefaultSettings

  lazy val presentationCompilerSettings = {
    val mtagsVersion = "1.6.2"
    Seq(
      libraryDependencies ++= Seq(
        "org.lz4" % "lz4-java" % "1.8.0",
        "io.get-coursier" % "interface" % "1.0.18",
        "org.scalameta" % "mtags-interfaces" % mtagsVersion,
        "com.google.guava" % "guava" % "33.2.1-jre",
      ),
      libraryDependencies += ("org.scalameta" % "mtags-shared_2.13.16" % mtagsVersion % SourceDeps),
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      scalacOptions ++= Seq("-source", "3.3"), // To avoid fatal migration warnings
      publishLocal := publishLocal.dependsOn( // It is best to publish all together. It is not rare to make changes in both compiler / presentation compiler and it can get misaligned
        `scala3-compiler-bootstrapped` / publishLocal,
        `scala3-library-bootstrapped` / publishLocal,
      ).value,
      Compile / scalacOptions ++= Seq("-Yexplicit-nulls", "-Wsafe-init"),
      Compile / sourceGenerators += Def.task {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val targetDir = (Compile/sourceManaged).value / "mtags-shared"

        val report = updateClassifiers.value
        val mtagsSharedSourceJar = report.select(
          configuration = configurationFilter("sourcedeps"),
          module = (_: ModuleID).name.startsWith("mtags-shared_"),
          artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
            sys.error(s"Could not fetch mtags-shared sources")

          }
        FileFunction.cached(cacheDir / s"fetchMtagsSharedSource",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info(s"Unpacking mtags-shared sources to $targetDir...")
          if (targetDir.exists)
            IO.delete(targetDir)
          IO.createDirectory(targetDir)
          IO.unzip(mtagsSharedSourceJar, targetDir)

          val mtagsSharedSources = (targetDir ** "*.scala").get.toSet
          mtagsSharedSources.foreach(f => {
            val lines = IO.readLines(f)
            val substitutions = (replaceProtobuf(_)) andThen (insertUnsafeNullsImport(_))
            IO.writeLines(f, substitutions(lines))
          })
          mtagsSharedSources
        } (Set(mtagsSharedSourceJar)).toSeq
      }.taskValue,
    )
  }

  lazy val `scala3-presentation-compiler-testcases` = project.in(file("presentation-compiler-testcases"))
    .dependsOn(`scala3-compiler-bootstrapped`)
    .settings(commonBootstrappedSettings)

  lazy val `scala3-language-server` = project.in(file("language-server")).
    dependsOn(dottyCompiler(Bootstrapped)).
    settings(commonBootstrappedSettings).
    settings(
      libraryDependencies ++= Seq(
        "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.6.0",
        Dependencies.`jackson-databind`
      ),
      // Work around https://github.com/eclipse/lsp4j/issues/295
      dependencyOverrides += "org.eclipse.xtend" % "org.eclipse.xtend.lib" % "2.16.0",
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value,
    ).
    settings(
      ideTestsCompilerVersion := (`scala3-compiler` / version).value,
      ideTestsCompilerArguments := Seq(),
      ideTestsDependencyClasspath := {
        val dottyLib = (`scala3-library-bootstrapped` / Compile / classDirectory).value
        val scalaLib =
          (`scala3-library-bootstrapped` / Compile / dependencyClasspath)
            .value
            .map(_.data)
            .filter(_.getName.matches("scala-library.*\\.jar"))
            .toList
        dottyLib :: scalaLib
      },
      Test / buildInfoKeys := Seq[BuildInfoKey](
        ideTestsCompilerVersion,
        ideTestsCompilerArguments,
        ideTestsDependencyClasspath
      ),
      Test / buildInfoPackage := "dotty.tools.languageserver.util.server",
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
    enablePlugins(DottyJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(
      commonBootstrappedSettings,
      // Required to run Scala.js tests.
      Test / fork := false,

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
    enablePlugins(DottyJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(
      commonBootstrappedSettings,
      bspEnabled := false,
      scalacOptions --= Seq("-Werror", "-deprecation"),

      // Required to run Scala.js tests.
      Test / fork := false,

      fetchScalaJSSource / sourceDirectory := target.value / s"scala-js-src-$scalaJSVersion",

      fetchScalaJSSource := {
        import org.eclipse.jgit.api._
        import org.eclipse.jgit.lib._

        val s = streams.value
        val ver = scalaJSVersion
        val trgDir = (fetchScalaJSSource / sourceDirectory).value

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
        ("org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion).cross(CrossVersion.for3Use2_13),

      (Compile / sourceGenerators) += Def.task {
        import org.scalajs.linker.interface.CheckedBehavior

        val stage = scalaJSStage.value

        val linkerConfig = stage match {
          case FastOptStage => (Compile / fastOptJS / scalaJSLinkerConfig).value
          case FullOptStage => (Compile / fullOptJS / scalaJSLinkerConfig).value
        }

        val moduleKind = linkerConfig.moduleKind
        val sems = linkerConfig.semantics

        ConstantHolderGenerator.generate(
            (Compile / sourceManaged).value,
            "org.scalajs.testsuite.utils.BuildInfo",
            "scalaVersion" -> scalaVersion.value,
            "hasSourceMaps" -> false, //DottyJSPlugin.wantSourceMaps.value,
            "isNoModule" -> (moduleKind == ModuleKind.NoModule),
            "isESModule" -> (moduleKind == ModuleKind.ESModule),
            "isCommonJSModule" -> (moduleKind == ModuleKind.CommonJSModule),
            "usesClosureCompiler" -> linkerConfig.closureCompiler,
            "hasMinifiedNames" -> (linkerConfig.closureCompiler || linkerConfig.minify),
            "compliantAsInstanceOfs" -> (sems.asInstanceOfs == CheckedBehavior.Compliant),
            "compliantArrayIndexOutOfBounds" -> (sems.arrayIndexOutOfBounds == CheckedBehavior.Compliant),
            "compliantArrayStores" -> (sems.arrayStores == CheckedBehavior.Compliant),
            "compliantNegativeArraySizes" -> (sems.negativeArraySizes == CheckedBehavior.Compliant),
            "compliantNullPointers" -> (sems.nullPointers == CheckedBehavior.Compliant),
            "compliantStringIndexOutOfBounds" -> (sems.stringIndexOutOfBounds == CheckedBehavior.Compliant),
            "compliantModuleInit" -> (sems.moduleInit == CheckedBehavior.Compliant),
            "productionMode" -> sems.productionMode,
            "esVersion" -> linkerConfig.esFeatures.esVersion.edition,
            "useECMAScript2015Semantics" -> linkerConfig.esFeatures.useECMAScript2015Semantics,
            "isWebAssembly" -> linkerConfig.experimentalUseWebAssembly,
        )
      }.taskValue,

      (Test / scalacOptions) += "-scalajs-genStaticForwardersForNonTopLevelObjects",

      scalaJSLinkerConfig ~= { _.withSemantics(build.TestSuiteLinkerOptions.semantics _) },
      (Test / scalaJSModuleInitializers) ++= build.TestSuiteLinkerOptions.moduleInitializers,

      // Perform Ycheck after the Scala.js-specific transformation phases
      scalacOptions += "-Ycheck:prepjsinterop,explicitJSClasses,addLocalJSFakeNews",

      Test / jsEnvInput := {
        val resourceDir = fetchScalaJSSource.value / "test-suite/js/src/test/resources"
        val f = (resourceDir / "NonNativeJSTypeTestNatives.js").toPath
        org.scalajs.jsenv.Input.Script(f) +: (Test / jsEnvInput).value
      },

      Test / unmanagedSourceDirectories ++= {
        val linkerConfig = scalaJSStage.value match {
          case FastOptStage => (Test / fastLinkJS / scalaJSLinkerConfig).value
          case FullOptStage => (Test / fullLinkJS / scalaJSLinkerConfig).value
        }

        if (linkerConfig.moduleKind != ModuleKind.NoModule && !linkerConfig.closureCompiler)
          Seq(baseDirectory.value / "test-require-multi-modules")
        else
          Nil
      },

      (Compile / managedSources) ++= {
        val dir = fetchScalaJSSource.value
        (
          (dir / "test-suite/js/src/main/scala" ** (("*.scala": FileFilter)
            -- "Typechecking*.scala" // defines a Scala 2 macro
            )).get

          ++ (dir / "junit-async/js/src/main/scala" ** "*.scala").get
        )
      },

      // A first excludelist of tests for those that do not compile or do not link
      (Test / managedSources) ++= {
        val dir = fetchScalaJSSource.value / "test-suite"

        val linkerConfig = scalaJSStage.value match {
          case FastOptStage => (Test / fastLinkJS / scalaJSLinkerConfig).value
          case FullOptStage => (Test / fullLinkJS / scalaJSLinkerConfig).value
        }

        val moduleKind = linkerConfig.moduleKind
        val hasModules = moduleKind != ModuleKind.NoModule

        def conditionally(cond: Boolean, subdir: String): Seq[File] =
          if (!cond) Nil
          else (dir / subdir ** "*.scala").get

        (
          (dir / "shared/src/test/scala" ** (("*.scala": FileFilter)
            -- "ReflectiveCallTest.scala" // uses many forms of structural calls that are not allowed in Scala 3 anymore
            -- "UTF16Test.scala" // refutable pattern match
            -- "CharsetTest.scala" // bogus @tailrec that Scala 2 ignores but Scala 3 flags as an error
            -- "ClassDiffersOnlyInCaseTest.scala" // looks like the Scala 3 compiler itself does not deal with that
            )).get

          ++ (dir / "shared/src/test/require-sam" ** "*.scala").get
          ++ (dir / "shared/src/test/require-jdk8" ** "*.scala").get
          ++ (dir / "shared/src/test/require-jdk7" ** "*.scala").get

          ++ (dir / "js/src/test/scala" ** (("*.scala": FileFilter)
            -- "StackTraceTest.scala" // would require `npm install source-map-support`
            -- "UnionTypeTest.scala" // requires the Scala 2 macro defined in Typechecking*.scala
            )).get

          ++ (dir / "js/src/test/require-2.12" ** "*.scala").get
          ++ (dir / "js/src/test/require-new-target" ** "*.scala").get
          ++ (dir / "js/src/test/require-sam" ** "*.scala").get
          ++ (dir / "js/src/test/scala-new-collections" ** "*.scala").get

          ++ conditionally(!hasModules, "js/src/test/require-no-modules")
          ++ conditionally(hasModules, "js/src/test/require-modules")
          ++ conditionally(hasModules && !linkerConfig.closureCompiler, "js/src/test/require-multi-modules")
          ++ conditionally(moduleKind == ModuleKind.ESModule, "js/src/test/require-dynamic-import")
          ++ conditionally(moduleKind == ModuleKind.ESModule, "js/src/test/require-esmodule")
        )
      },

      /* For some reason, in Scala 3, the implementation of IterableDefaultTest
       * resolves to `scala.collection.ArrayOps.ArrayIterator`, whose `next()`
       * method is not compliant when called past the last element on Scala.js.
       * It relies on catching an `ArrayIndexOutOfBoundsException`.
       * We have to ignore it here.
       */
      Test / testOptions := Seq(Tests.Filter(_ != "org.scalajs.testsuite.javalib.lang.IterableDefaultTest")),

      Test / managedResources ++= {
        val testDir = fetchScalaJSSource.value / "test-suite/js/src/test"

        val common = (testDir / "resources" ** "*.js").get

        val moduleSpecific = scalaJSLinkerConfig.value.moduleKind match {
          case ModuleKind.NoModule       => Nil
          case ModuleKind.CommonJSModule => (testDir / "resources-commonjs" ** "*.js").get
          case ModuleKind.ESModule       => (testDir / "resources-esmodule" ** "*.js").get
        }

        common ++ moduleSpecific
      },
    )

  lazy val sjsCompilerTests = project.in(file("sjs-compiler-tests")).
    dependsOn(`scala3-compiler` % "test->test").
    settings(
      commonNonBootstrappedSettings,

      libraryDependencies ++= Seq(
        "org.scala-js" %% "scalajs-linker" % scalaJSVersion % Test cross CrossVersion.for3Use2_13,
        "org.scala-js" %% "scalajs-env-nodejs" % "1.3.0" % Test cross CrossVersion.for3Use2_13,
      ),

      // Change the baseDirectory when running the tests
      Test / baseDirectory := baseDirectory.value.getParentFile,

      javaOptions ++= (`scala3-compiler` / javaOptions).value,
      javaOptions ++= {
        val externalJSDeps = (`scala3-library-bootstrappedJS` / Compile / externalDependencyClasspath).value
        val dottyLibraryJSJar = (`scala3-library-bootstrappedJS` / Compile / packageBin).value.getAbsolutePath

        Seq(
          "-Ddotty.tests.classes.dottyLibraryJS=" + dottyLibraryJSJar,
          "-Ddotty.tests.classes.scalaJSJavalib=" + findArtifactPath(externalJSDeps, "scalajs-javalib"),
          "-Ddotty.tests.classes.scalaJSScalalib=" + findArtifactPath(externalJSDeps, "scalajs-scalalib_2.13"),
          "-Ddotty.tests.classes.scalaJSLibrary=" + findArtifactPath(externalJSDeps, "scalajs-library_2.13"),
        )
      },
    )

  lazy val `scala3-bench` = project.in(file("bench")).asDottyBench(NonBootstrapped)
  lazy val `scala3-bench-bootstrapped` = project.in(file("bench")).asDottyBench(Bootstrapped)
  lazy val `scala3-bench-run` = project.in(file("bench-run")).asDottyBench(Bootstrapped)

  lazy val `scala3-bench-micro` = project.in(file("bench-micro"))
    .asDottyBench(Bootstrapped)
    .settings(Jmh / run / mainClass := Some("org.openjdk.jmh.Main"))

  val testcasesOutputDir = taskKey[Seq[String]]("Root directory where tests classes are generated")
  val testcasesSourceRoot = taskKey[String]("Root directory where tests sources are generated")
  val testDocumentationRoot = taskKey[String]("Root directory where tests documentation are stored")
  val generateSelfDocumentation = taskKey[Unit]("Generate example documentation")
  // Note: the two tasks below should be one, but a bug in Tasty prevents that
  val generateScalaDocumentation = inputKey[Unit]("Generate documentation for dotty lib")
  val generateStableScala3Documentation  = inputKey[Unit]("Generate documentation for stable dotty lib")
  val generateTestcasesDocumentation  = taskKey[Unit]("Generate documentation for testcases, useful for debugging tests")

  val generateReferenceDocumentation = inputKey[Unit]("Generate language reference documentation for Scala 3")

  lazy val `scaladoc-testcases` = project.in(file("scaladoc-testcases")).
    dependsOn(`scala3-compiler-bootstrapped`).
    settings(commonBootstrappedSettings)


  /**
   * Collection of projects building targets for scaladoc, these are:
   * - common - common module for javascript
   * - main - main target for default scaladoc producing html webpage
   * - contributors - not related project to any of forementioned modules. Used for presenting contributors for static site.
   *                   Made as an indepented project to be scaladoc-agnostic.
   */
  lazy val `scaladoc-js-common` = project.in(file("scaladoc-js/common")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scala3-library-bootstrappedJS`).
    settings(
      commonBootstrappedSettings,
      libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "2.8.0"))

  lazy val `scaladoc-js-main` = project.in(file("scaladoc-js/main")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scaladoc-js-common`).
    settings(
      commonBootstrappedSettings,
      scalaJSUseMainModuleInitializer := true,
      Test / fork := false
    )

  lazy val `scaladoc-js-contributors` = project.in(file("scaladoc-js/contributors")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scaladoc-js-common`).
    settings(
      commonBootstrappedSettings,
      Test / fork := false,
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "2.8.0")
    )

  def generateDocumentation(configTask: Def.Initialize[Task[GenerationConfig]]) =
    Def.taskDyn {
      val config = configTask.value
      config.get[OutputDir].foreach { outDir =>
        IO.createDirectory(file(outDir.value))
      }
      val command = generateCommand(config)
      Def.task {
        (Compile / run).toTask(command).value
      }
    }

  def generateStaticAssetsTask = Def.task {
    DocumentationWebsite.generateStaticAssets(
      (`scaladoc-js-contributors` / Compile / fullOptJS).value.data,
      (`scaladoc-js-main` / Compile / fullOptJS).value.data,
      (`scaladoc-js-contributors` / Compile / baseDirectory).value / "css",
      (`scaladoc-js-common` / Compile / baseDirectory).value / "css",
      (Compile / resourceManaged).value,
    )
  }

  def bundleCSS = Def.task {
    val unmanagedResources = (Compile / resourceDirectory).value
    def createBundle(dir: File): Seq[File] = {
      val (dirs, files) = IO.listFiles(dir).toList.partition(_.isDirectory)
      val targetDir = (Compile / resourceManaged).value.toPath.resolve(unmanagedResources.toPath.relativize(dir.toPath)).toFile
      val bundleFile = targetDir / "bundle.css"
      if (bundleFile.exists) bundleFile.delete()
      files.foreach(file => IO.append(bundleFile, IO.readBytes(file)))
      bundleFile :: dirs.flatMap(createBundle)
    }

    val cssThemePath = unmanagedResources / "dotty_res" / "styles" / "theme"

    createBundle(cssThemePath)
  }

  val SourceLinksIntegrationTest = config("sourceLinksIntegrationTest") extend Test

  lazy val scaladoc = project.in(file("scaladoc")).
    configs(SourceLinksIntegrationTest).
    settings(commonBootstrappedSettings).
    dependsOn(`scala3-compiler-bootstrapped`).
    dependsOn(`scala3-tasty-inspector`).
    settings(inConfig(SourceLinksIntegrationTest)(Defaults.testSettings)).
    settings(
      SourceLinksIntegrationTest / scalaSource := baseDirectory.value / "test-source-links",
      SourceLinksIntegrationTest / test:= ((SourceLinksIntegrationTest / test) dependsOn generateScalaDocumentation.toTask("")).value,
    ).
    settings(
      scalacOptions += "-experimental" // workaround use of experimental .info in Scaladoc2AnchorCreator
    ).
    settings(
      Compile / resourceGenerators ++= Seq(
        generateStaticAssetsTask.taskValue,
        bundleCSS.taskValue
      ),
      libraryDependencies ++= Dependencies.flexmarkDeps ++ Seq(
        "nl.big-o" % "liqp" % "0.8.2",
        "org.jsoup" % "jsoup" % "1.17.2", // Needed to process .html files for static site
        Dependencies.`jackson-dataformat-yaml`,

        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      ),
      Compile / mainClass := Some("dotty.tools.scaladoc.Main"),
      Compile / buildInfoKeys := Seq[BuildInfoKey](version),
      Compile / buildInfoPackage := "dotty.tools.scaladoc",
      BuildInfoPlugin.buildInfoScopedSettings(Compile),
      BuildInfoPlugin.buildInfoDefaultSettings,

      Test / test := (Test / test).dependsOn(`scaladoc-testcases` / Compile / compile).value,
      Test / testcasesOutputDir := (`scaladoc-testcases`/Compile/products).value.map(_.getAbsolutePath),
      Test / testcasesSourceRoot := ((`scaladoc-testcases` / baseDirectory).value / "src").getAbsolutePath.toString,
      run / baseDirectory := (ThisBuild / baseDirectory).value,
      generateSelfDocumentation := Def.taskDyn {
        generateDocumentation(Scaladoc)
      }.value,

      generateScalaDocumentation := Def.inputTaskDyn {
        val majorVersion = (`scala-library-bootstrapped` / scalaBinaryVersion).value

        val extraArgs = spaceDelimited("[<output-dir>] [--justAPI]").parsed
        val outputDirOverride = extraArgs.headOption.fold(identity[GenerationConfig](_))(newDir => {
          config: GenerationConfig => config.add(OutputDir(newDir))
        })
        val justAPIArg: Option[String] = extraArgs.drop(1).find(_ == "--justAPI")
        val justAPI = justAPIArg.fold(identity[GenerationConfig](_))(_ => {
          config: GenerationConfig => config.remove[SiteRoot]
        })
        val overrideFunc = outputDirOverride.andThen(justAPI)

        val config = Def.task {
          overrideFunc(Scala3.value)
        }

        val writeAdditionalFiles = Def.task {
          val dest = file(config.value.get[OutputDir].get.value)
          if (justAPIArg.isEmpty) {
            IO.write(dest / "versions" / "latest-nightly-base", majorVersion)
            // This file is used by GitHub Pages when the page is available in a custom domain
            IO.write(dest / "CNAME", "dotty.epfl.ch")
          }
        }

        writeAdditionalFiles.dependsOn(generateDocumentation(config))
      }.evaluated,

      generateStableScala3Documentation := Def.inputTaskDyn {
        val extraArgs = spaceDelimited("<version>").parsed
        val config = stableScala3(extraArgs.head)
        generateDocumentation(config)
      }.evaluated,

      generateTestcasesDocumentation := Def.taskDyn {
        generateDocumentation(Testcases)
      }.value,

      // Generate the Scala 3 reference documentation (published at https://docs.scala-lang.org/scala3/reference)
      generateReferenceDocumentation := Def.inputTaskDyn {
        val shouldRegenerateExpectedLinks = (Space ~> literal("--no-regenerate-expected-links")).?.parsed.isEmpty

        generateStaticAssetsTask.value

        // Move all the source files to a temporary directory and apply some changes specific to the reference documentation
        val temp = IO.createTemporaryDirectory
        IO.copyDirectory(file("docs"), temp / "docs")
        IO.delete(temp / "docs" / "_blog")

        // Overwrite the main layout and the sidebar
        IO.copyDirectory(
          file("project") / "resources" / "referenceReplacements",
          temp / "docs",
          overwrite = true
        )

        // Add redirections from previously supported URLs, for some pages
        for (name <- Seq("changed-features", "contextual", "dropped-features", "metaprogramming", "other-new-features")) {
          val path = temp / "docs" / "_docs" / "reference" / name / s"${name}.md"
          val contentLines = IO.read(path).linesIterator.to[collection.mutable.ArrayBuffer]
          contentLines.insert(1, s"redirectFrom: /${name}.html") // Add redirection
          val newContent = contentLines.mkString("\n")
          IO.write(path, newContent)
        }

        val languageReferenceConfig = Def.task {
          Scala3.value
            .add(OutputDir("scaladoc/output/reference"))
            .add(SiteRoot(s"${temp.getAbsolutePath}/docs"))
            .add(ProjectName("Scala 3 Reference"))
            .add(ProjectVersion(baseVersion))
            .remove[VersionsDictionaryUrl]
            .add(SourceLinks(List(
              s"${temp.getAbsolutePath}=github://scala/scala3/language-reference-stable"
            )))
            .withTargets(List("___fake___.scala"))
        }

        val expectedLinksRegeneration = Def.task {
          if (shouldRegenerateExpectedLinks) {
            val script = (file("project") / "scripts" / "regenerateExpectedLinks").toString
            val outputDir = languageReferenceConfig.value.get[OutputDir].get.value
            val expectedLinksFile = (file("project") / "scripts" / "expected-links" / "reference-expected-links.txt").toString
            import _root_.scala.sys.process._
            s"$script $outputDir $expectedLinksFile".!
          }
        }

        expectedLinksRegeneration.dependsOn(generateDocumentation(languageReferenceConfig))
      }.evaluated,

      Test / buildInfoKeys := Seq[BuildInfoKey](
        (Test / Build.testcasesOutputDir),
        (Test / Build.testcasesSourceRoot),
        Build.testDocumentationRoot,
      ),
      testDocumentationRoot := (baseDirectory.value / "test-documentations").getAbsolutePath,
      Test / buildInfoPackage := "dotty.tools.scaladoc.test",
      BuildInfoPlugin.buildInfoScopedSettings(Test),
    )

  // various scripted sbt tests
  lazy val `sbt-test` = project.in(file("sbt-test")).
    enablePlugins(ScriptedPlugin).
    settings(commonSettings).
    settings(
      sbtTestDirectory := baseDirectory.value,
      target := baseDirectory.value / ".." / "out" / name.value,

      // The batch mode accidentally became the default with no way to disable
      // it in sbt 1.4 (https://github.com/sbt/sbt/issues/5913#issuecomment-716003195).
      // We enable it explicitly here to make it clear that we're using it.
      scriptedBatchExecution := true,

      scriptedLaunchOpts ++= Seq(
        "-Dplugin.version=" + version.value,
        "-Dplugin.scalaVersion=" + dottyVersion,
        "-Dplugin.scala2Version=" + stdlibVersion(Bootstrapped),
        "-Dplugin.scalaJSVersion=" + scalaJSVersion,
        "-Dsbt.boot.directory=" + ((ThisBuild / baseDirectory).value / ".sbt-scripted").getAbsolutePath // Workaround sbt/sbt#3469
      ),
      // Pass along ivy home and repositories settings to sbt instances run from the tests
      scriptedLaunchOpts ++= {
        val repositoryPath = (io.Path.userHome / ".sbt" / "repositories").absolutePath
        s"-Dsbt.repository.config=$repositoryPath" ::
        ivyPaths.value.ivyHome.map("-Dsbt.ivy.home=" + _.getAbsolutePath).toList
      },
      scriptedBufferLog := true,
      scripted := scripted.dependsOn(
        (`scala3-sbt-bridge` / publishLocalBin),
        (`scala3-interfaces` / publishLocalBin),
        (`scala3-compiler-bootstrapped` / publishLocalBin),
        (`scala3-library-bootstrapped` / publishLocalBin),
        (`scala3-library-bootstrappedJS` / publishLocalBin),
        (`tasty-core-bootstrapped` / publishLocalBin),
        (`scala3-staging` / publishLocalBin),
        (`scala3-tasty-inspector` / publishLocalBin),
        (`scaladoc` / publishLocalBin),
        (`scala3-bootstrapped` / publishLocalBin) // Needed because sbt currently hardcodes the dotty artifact
      ).evaluated
    )

  val prepareCommunityBuild = taskKey[Unit]("Publish local the compiler and the sbt plugin. Also store the versions of the published local artefacts in two files, community-build/{scala3-bootstrapped.version,sbt-injected-plugins}.")

  lazy val `community-build` = project.in(file("community-build")).
    dependsOn(dottyLibrary(Bootstrapped)).
    settings(commonBootstrappedSettings).
    settings(
      prepareCommunityBuild := {
        (`scala3-sbt-bridge` / publishLocal).value
        (`scala3-interfaces` / publishLocal).value
        (`tasty-core-bootstrapped` / publishLocal).value
        (`scala3-library-bootstrapped` / publishLocal).value
        (`scala3-tasty-inspector` / publishLocal).value
        (`scaladoc` / publishLocal).value
        (`scala3-compiler-bootstrapped` / publishLocal).value
        (`scala3-bootstrapped` / publishLocal).value
        (`scala3-library-bootstrappedJS` / publishLocal).value
        // (publishLocal in `scala3-staging`).value
        val pluginText =
          s"""addSbtPlugin("org.scala-js" % "sbt-scalajs" % "$scalaJSVersion")"""
        IO.write(baseDirectory.value / "sbt-injected-plugins", pluginText)
        IO.write(baseDirectory.value / "scala3-bootstrapped.version", dottyVersion)
      },
      (Test / testOptions) += Tests.Argument(
        TestFrameworks.JUnit,
        "--include-categories=dotty.communitybuild.TestCategory",
        "--run-listener=dotty.communitybuild.FailureSummarizer",
      ),
      Compile/run := (Compile/run).dependsOn(prepareCommunityBuild).evaluated,
      Test / testOnly := ((Test / testOnly) dependsOn prepareCommunityBuild).evaluated,
      Test / test     := ((Test / test    ) dependsOn prepareCommunityBuild).value,
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
    publishTo := {
      if (sys.env.get("NEWNIGHTLY").contains("yes")) {
        Some(sys.env("MAVEN_REPOSITORY_REALM") at sys.env("MAVEN_REPOSITORY_URL"))
      } else if (isSnapshot.value) {
        Some("central-snapshots" at "https://central.sonatype.com/repository/maven-snapshots/")
      } else
        localStaging.value
    },
    credentials ++= (
      if (sys.env.get("NEWNIGHTLY").contains("yes")) {
        for {
          username <- sys.env.get("MAVEN_REPOSITORY_USER")
          token <- sys.env.get("MAVEN_REPOSITORY_TOKEN")
        } yield Credentials(sys.env("MAVEN_REPOSITORY_REALM"), sys.env("MAVEN_REPOSITORY_HOST"), username, token)
      }
      else
        // The old build credentials are configured differently
        None
    ).toList,
    publishConfiguration ~= (_.withOverwrite(true)),
    publishLocalConfiguration ~= (_.withOverwrite(true)),
    projectID ~= {id =>
      val line = "scala.versionLine" -> versionLine
      id.withExtraAttributes(id.extraAttributes + line)
    },
    Test / publishArtifact := false,
    homepage := Some(url(dottyGithubUrl)),
    licenses += (("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))),
    scmInfo := Some(ScmInfo(url(dottyGithubUrl), "scm:git:git@github.com:scala/scala3.git")),
    developers := List(
      Developer(
        id = "scala",
        name = "The Scala Team",
        email = "security@scala-lang.org",
        url = url("https://scala-lang.org")
      )
    ),
  )

  lazy val commonDistSettings = Seq(
    publishArtifact := false,
    republishRepo := target.value / "republish",
    Universal / packageName := packageName.value,
    // ========
    Universal / stage := (Universal / stage).dependsOn(republish).value,
    Universal / packageBin := (Universal / packageBin).dependsOn(republish).value,
    Universal / packageZipTarball := (Universal / packageZipTarball).dependsOn(republish)
      .map { archiveFile =>
        // Rename .tgz to .tar.gz for consistency with previous versions
        val renamedFile = archiveFile.getParentFile() / archiveFile.getName.replaceAll("\\.tgz$", ".tar.gz")
        IO.move(archiveFile, renamedFile)
        renamedFile
      }
      .value,
    // ========
    Universal / mappings ++= directory(dist.base / "bin"),
    Universal / mappings ++= directory(republishRepo.value / "maven2"),
    Universal / mappings ++= directory(republishRepo.value / "lib"),
    Universal / mappings ++= directory(republishRepo.value / "libexec"),
    Universal / mappings +=  (republishRepo.value / "VERSION") -> "VERSION",
    // ========
    republishCommandLibs += ("scala" -> List("scala3-interfaces", "scala3-compiler", "scala3-library", "tasty-core")),
    republishCommandLibs += ("with_compiler" -> List("scala3-staging", "scala3-tasty-inspector", "^!scala3-interfaces", "^!scala3-compiler", "^!scala3-library", "^!tasty-core")),
    republishCommandLibs += ("scaladoc" -> List("scala3-interfaces", "scala3-compiler", "scala3-library", "tasty-core", "scala3-tasty-inspector", "scaladoc")),
  )

  lazy val dist = project.asDist(Bootstrapped)
    .settings(packageName := "scala3-" + dottyVersion)
    .settings(
      republishLibexecDir := baseDirectory.value / "libexec",
      republishCoursier +=
        ("coursier.jar" -> s"https://github.com/coursier/coursier/releases/download/v$coursierJarVersion/coursier.jar"),
      republishLaunchers +=
        ("scala-cli.jar" -> s"https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli.jar"),
    )

  lazy val `dist-mac-x86_64` = project.in(file("dist/mac-x86_64")).asDist(Bootstrapped)
    .settings(packageName := (dist / packageName).value + "-x86_64-apple-darwin")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli" -> s"gz+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-x86_64-apple-darwin.gz")
    )

  lazy val `dist-mac-aarch64` = project.in(file("dist/mac-aarch64")).asDist(Bootstrapped)
    .settings(packageName := (dist / packageName).value + "-aarch64-apple-darwin")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli" -> s"gz+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-aarch64-apple-darwin.gz")
    )

  lazy val `dist-win-x86_64` = project.in(file("dist/win-x86_64")).asDist(Bootstrapped)
    .enablePlugins(WindowsPlugin) // TO GENERATE THE `.msi` installer
    .settings(packageName := (dist / packageName).value + "-x86_64-pc-win32")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli.exe" -> s"zip+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-x86_64-pc-win32.zip!/scala-cli.exe")
    )
    .settings(
      Windows / name := "scala",
      // Windows/version is used to create ProductInfo - it requires a version without any -RC suffixes
      // If not explicitly overriden it would try to use `dottyVersion` assigned to `dist-win-x86_64/version`
      Windows / version    := developedVersion,
      Windows / mappings   := (Universal / mappings).value,
      Windows / packageBin := (Windows / packageBin).dependsOn(republish).value,
      Windows / wixFiles   := (Windows / wixFiles).dependsOn(republish).value,
      // Additional information: https://wixtoolset.org/docs/schema/wxs/package/
      maintainer := "The Scala Programming Language",                             // The displayed maintainer of the package
      packageSummary := s"Scala $dottyVersion",                                   // The displayed name of the package
      packageDescription := """Installer for the Scala Programming Language""",   // The displayed description of the package
      wixProductId := "*",                                                        // Unique ID for each generated MSI; will change for each generated msi
      wixProductUpgradeId := "3E5A1A82-CA67-4353-94FE-5BDD400AF66B",              // Unique ID to identify the package; used to manage the upgrades
      wixProductLicense := Some(dist.base / "LICENSE.rtf")                        // Link to the LICENSE to show during the installation (keep in sync with ../LICENSE)
    )

  lazy val `dist-linux-x86_64` = project.in(file("dist/linux-x86_64")).asDist(Bootstrapped)
    .settings(packageName := (dist / packageName).value + "-x86_64-pc-linux")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli" -> s"gz+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-x86_64-pc-linux.gz")
    )

  lazy val `dist-linux-aarch64` = project.in(file("dist/linux-aarch64")).asDist(Bootstrapped)
    .settings(packageName := (dist / packageName).value + "-aarch64-pc-linux")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli" -> s"gz+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-aarch64-pc-linux.gz")
    )

  private def customMimaReportBinaryIssues(issueFilterLocation: String) = mimaReportBinaryIssues := {
    mimaReportBinaryIssues.result.value match {
      case Inc(inc: Incomplete) =>
        streams.value.log.error(s"\nFilters in $issueFilterLocation are used in this check.\n ")
        throw inc
      case Value(v) => v
    }
  }

  implicit class ProjectDefinitions(val project: Project) extends AnyVal {

    // FIXME: we do not aggregate `bin` because its tests delete jars, thus breaking other tests
    def asDottyRoot(implicit mode: Mode): Project = project.withCommonSettings.
      aggregate(`scala3-interfaces`, dottyLibrary, dottyCompiler, tastyCore, `scala3-sbt-bridge`).
      bootstrappedAggregate(`scala3-language-server`, `scala3-staging`,
        `scala3-tasty-inspector`, `scala3-library-bootstrappedJS`, scaladoc, `scala3-presentation-compiler`).
      dependsOn(tastyCore).
      dependsOn(dottyCompiler).
      dependsOn(dottyLibrary).
      bootstrappedSettings(
        addCommandAlias("clean", ";scala3-bootstrapped/clean"),
      ).
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
        repl := (`scala3-compiler-bootstrapped` / repl).value,
        buildQuick := {
          val _ = (`scala3-compiler` / Compile / compile).value
          val cp = (`scala3-compiler` / Compile / fullClasspath).value.map(_.data.getAbsolutePath).mkString(File.pathSeparator)
          IO.write(baseDirectory.value / "bin" / ".cp", cp)
        },
        (Compile / console) := (Compile / console).dependsOn(Def.task {
          import _root_.scala.io.AnsiColor._
          val msg = "`console` uses the reference Scala version. Use `repl` instead."
          val f = "═" * (msg.length + 2)
          val box =
            s"""╔$f╗
               |║ ${BOLD}$msg$RESET ║
               |╚$f╝""".stripMargin
          streams.value.log.warn(box)
        }).value,
      ).
      settings(
        publish / skip := true
      )

    def asDottyCompiler(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(`scala3-interfaces`).
      dependsOn(dottyLibrary).
      dependsOn(tastyCore).
      settings(dottyCompilerSettings)

    def asDottyLibrary(implicit mode: Mode): Project = {
      val base = project
        .withCommonSettings
        .settings(dottyLibrarySettings)
      if (mode == Bootstrapped) {
        base.settings(
          (Compile/doc) := {
            // Workaround for
            // [error]    |object IArray cannot have the same name as object IArray in package scala
            // -- cannot define object member with the same name as a object member in self reference _.
            val doWork = (Compile/doc).result.value
            (Compile/doc/target).value
          },
        )
      } else base
    }


    def asTastyCore(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyLibrary).
      settings(tastyCoreSettings).
      settings(disableDocSetting).
      settings(
        versionScheme := Some("semver-spec"),
        Test / envVars ++= Map(
          "EXPECTED_TASTY_VERSION" -> expectedTastyVersion,
        ),
      )

    def asTastyCoreScala2: Project = project
      .settings(commonScala2Settings)
      // need to add @annotation.internal.sharable to the classpath for compiling
      // we don't actually publish this library anywhere, so it's fine.
      // if someone depends on the sources of tasty-core in a scala 2 project,
      // they should strip the sharable annotation, or add -Ytasty-reader
      .dependsOn(dottyLibrary(NonBootstrapped) % Provided)
      .settings(
        scalacOptions += "-Ytasty-reader" // to read scala3 library
      )

    def asDottyBench(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler).
      settings(commonBenchmarkSettings).
      enablePlugins(JmhPlugin)

    def asDist(implicit mode: Mode): Project = project.
      enablePlugins(UniversalPlugin, RepublishPlugin).
      withCommonSettings.
      settings(commonDistSettings).
      dependsOn(
        `scala3-interfaces`,
        dottyCompiler,
        dottyLibrary,
        tastyCore,
        `scala3-staging`,
        `scala3-tasty-inspector`,
        scaladoc,
        `scala3-sbt-bridge`, // for scala-cli
      ).
      bootstrappedSettings(
        target := baseDirectory.value / "target" // override setting in commonBootstrappedSettings
      )

    def withCommonSettings(implicit mode: Mode): Project = project.settings(mode match {
      case NonBootstrapped => commonNonBootstrappedSettings
      case Bootstrapped => commonBootstrappedSettings
    })

  }

  /* Tests TASTy version invariants during NIGHLY, RC or Stable releases */
  def checkReleasedTastyVersion(): Unit = {
    case class ScalaVersion(minor: Int, patch: Int, isRC: Boolean)
    def parseScalaVersion(version: String): ScalaVersion = version.split("\\.|-").take(4) match {
      case Array("3", minor, patch)    => ScalaVersion(minor.toInt, patch.toInt, false)
      case Array("3", minor, patch, _) => ScalaVersion(minor.toInt, patch.toInt, true)
      case other => sys.error(s"Invalid Scala base version string: $baseVersion")
    }
    lazy val version = parseScalaVersion(baseVersion)
    lazy val referenceV = parseScalaVersion(referenceVersion)
    lazy val (tastyMinor, tastyIsExperimental) = expectedTastyVersion.split("\\.|-").take(4) match {
      case Array("28", minor)                    => (minor.toInt, false)
      case Array("28", minor, "experimental", _) => (minor.toInt, true)
      case other => sys.error(s"Invalid TASTy version string: $expectedTastyVersion")
    }

    if(isNightly) {
      assert(tastyIsExperimental, "TASTY needs to be experimental in nightly builds")
      val expectedTastyMinor = version.patch match {
        case 0 => version.minor
        case 1 if referenceV.patch == 0 && referenceV.isRC =>
          // Special case for a period when reference version is a new unstable minor
          // Needed for non_bootstrapped tests requiring either stable tasty or the same experimental version produced by both reference and bootstrapped compiler
          assert(version.minor == referenceV.minor, "Expected reference and base version to use the same minor")
          version.minor
        case _ => version.minor + 1
      }
      assert(tastyMinor == expectedTastyMinor, "Invalid TASTy minor version")
    }

    if(isRelease) {
      assert(version.minor == tastyMinor, "Minor versions of TASTY vesion and Scala version should match in release builds")
      assert(!referenceV.isRC, "Stable release needs to use stable compiler version")
      if (version.isRC && version.patch == 0)
        assert(tastyIsExperimental, "TASTy should be experimental when releasing a new minor version RC")
      else
        assert(!tastyIsExperimental, "Stable version cannot use experimental TASTY")
    }
  }
}

object ScaladocConfigs {
  import Build._
  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  def dottyExternalMapping = ".*scala/.*::scaladoc3::https://dotty.epfl.ch/api/"
  def javaExternalMapping = ".*java/.*::javadoc::https://docs.oracle.com/javase/8/docs/api/"
  def defaultSourceLinks(version: String) = {
    def dottySrcLink(v: String) = sys.env.get("GITHUB_SHA") match {
      case Some(sha) => s"github://scala/scala3/$sha"
      case None => s"github://scala/scala3/$v"
    }
    SourceLinks(List(dottySrcLink(version), "docs=github://scala/scala3/main#docs"))
  }

  lazy val DefaultGenerationSettings = Def.task {
    def projectVersion = version.value
    def socialLinks = SocialLinks(List(
      "github::https://github.com/scala/scala3",
      "discord::https://discord.com/invite/scala",
      "twitter::https://twitter.com/scala_lang",
    ))
    def projectLogo = ProjectLogo("docs/_assets/images/logo.svg")
    def skipByRegex = SkipByRegex(List(".+\\.internal($|\\..+)", ".+\\.impl($|\\..+)"))
    def skipById = SkipById(List(
      "scala.runtime.stdLibPatches",
      "scala.runtime.MatchCase",
    ))
    def projectFooter = ProjectFooter(s"Copyright (c) 2002-$currentYear, LAMP/EPFL")
    def defaultTemplate = DefaultTemplate("static-site-main")
    GenerationConfig(
      List(),
      ProjectVersion(projectVersion),
      GenerateInkuire(true),
      defaultSourceLinks(version = dottyVersion),
      skipByRegex,
      skipById,
      projectLogo,
      socialLinks,
      projectFooter,
      defaultTemplate,
      Author(true),
      Groups(true),
      QuickLinks(
        List(
          "Learn::https://docs.scala-lang.org/",
          "Install::https://www.scala-lang.org/download/",
          "Playground::https://scastie.scala-lang.org",
          "Find\u00A0A\u00A0Library::https://index.scala-lang.org",
          "Community::https://www.scala-lang.org/community/",
          "Blog::https://www.scala-lang.org/blog/",
        )
      )
    )
  }

  lazy val Scaladoc = Def.task {
    DefaultGenerationSettings.value
      .add(UseJavacp(true))
      .add(ProjectName("scaladoc"))
      .add(OutputDir("scaladoc/output/self"))
      .add(Revision(VersionUtil.gitHash))
      .add(ExternalMappings(List(dottyExternalMapping, javaExternalMapping)))
      .withTargets((Compile / classDirectory).value.getAbsolutePath :: Nil)
  }

  lazy val Testcases = Def.task {
    val tastyRoots = (Test / Build.testcasesOutputDir).value
    DefaultGenerationSettings.value
      .add(UseJavacp(true))
      .add(OutputDir("scaladoc/output/testcases"))
      .add(ProjectName("scaladoc testcases"))
      .add(Revision("main"))
      .add(SnippetCompiler(List("scaladoc-testcases/docs=compile")))
      .add(SiteRoot("scaladoc-testcases/docs"))
      .add(CommentSyntax(List(
        "scaladoc-testcases/src/example/comment-md=markdown",
        "scaladoc-testcases/src/example/comment-wiki=wiki"
      )))
      .add(ExternalMappings(List(dottyExternalMapping, javaExternalMapping)))
      .withTargets(tastyRoots)
  }

  lazy val Scala3 = Def.task {
    DefaultGenerationSettings.value
      .add(ProjectName("Scala 3"))
      .add(OutputDir(file("scaladoc/output/scala3").getAbsoluteFile.getAbsolutePath))
      .add(Revision("main"))
      .add(ExternalMappings(List(javaExternalMapping)))
      .add(DocRootContent(((`scala-library-bootstrapped` / baseDirectory).value / "src" / "rootdoc.txt").toString))
      .add(CommentSyntax(List(
        //s"${dottyLibRoot}=markdown",
        //s"${stdLibRoot}=wiki",
        "wiki"
      )))
      .add(VersionsDictionaryUrl("https://scala-lang.org/api/versions.json"))
      .add(DocumentSyntheticTypes(true))
      //.add(SnippetCompiler(List(
        //s"$dottyLibRoot/src/scala=compile",
        //s"$dottyLibRoot/src/scala/compiletime=compile",
        //s"$dottyLibRoot/src/scala/util=compile",
        //s"$dottyLibRoot/src/scala/util/control=compile"
      //)))
      .add(SiteRoot("docs"))
      .add(ApiSubdirectory(true))
      .withTargets((`scala-library-bootstrapped` / Compile / products).value.map(_.getAbsolutePath))
  }

  def stableScala3(version: String) = Def.task {
    val scalaLibrarySrc = s"out/bootstrap/scala2-library-bootstrapped/scala-$version-bin-SNAPSHOT-nonbootstrapped/src_managed"
    val dottyLibrarySrc = "library/src"
    Scala3.value
      .add(defaultSourceLinks(version = version))
      .add(ProjectVersion(version))
      .add(SnippetCompiler(
        List(
          s"$dottyLibrarySrc/scala/quoted=compile",
          s"$dottyLibrarySrc/scala/compiletime=compile",
          s"$dottyLibrarySrc/scala/util=compile",
          s"$dottyLibrarySrc/scala/util/control=compile"
        )
      ))
      .add(CommentSyntax(List(
        s"$dottyLibrarySrc=markdown",
        s"$scalaLibrarySrc=wiki",
        "wiki"
      )))
      .add(DocRootContent(s"$scalaLibrarySrc/rootdoc.txt"))
      .withTargets(
        Seq(
          s"tmp/interfaces/target/classes",
          s"out/bootstrap/tasty-core-bootstrapped/scala-$version-bin-SNAPSHOT-nonbootstrapped/classes"
        )
      )
      .remove[SiteRoot]
      .remove[ApiSubdirectory]
  }
}
