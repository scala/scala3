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
import dotty.tools.sbtplugin.ScalaLibraryPlugin.autoImport._
import dotty.tools.sbtplugin.DottyJSPlugin
import dotty.tools.sbtplugin.DottyJSPlugin.autoImport._

import sbt.plugins.SbtPlugin
import sbt.ScriptedPlugin.autoImport._
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import org.scalajs.linker.interface.ESVersion

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
  val referenceVersion = "3.8.0-RC5"

  /** Version of the Scala compiler targeted in the current release cycle
   *  Contains a version without RC/SNAPSHOT/NIGHTLY specific suffixes
   *  Should be updated ONLY after release or cutoff for previous release cycle.
   *
   *  Should only be referred from `dottyVersion` or settings/tasks requiring simplified version string,
   *  eg. `compatMode` or Windows native distribution version.
   *
   *  Warning: Change of this variable might require updating `expectedTastyVersion`
   */
  val developedVersion = "3.8.1"

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

  /** Minor version against which we check binary compatibility.
   *
   *  This must be the earliest published release in the same versioning line.
   *  For a developedVersion `3.M.P` the mimaPreviousDottyVersion should be set to:
   *   - `3.M.0`     if `P > 0`
   *   - `3.(M-1).0` if `P = 0`
   */
  val mimaPreviousDottyVersion = "3.8.0-RC1" // temporary until 3.8.0 is released

  /** Version of Scala CLI to download */
  val scalaCliLauncherVersion = "1.11.0"
  /** Version of Coursier to download for initializing the local maven repo of Scala command */
  val coursierJarVersion = "2.1.25-M21"

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

  val homepageUrl = "https://scala-lang.org/"
  val dottyOrganization = "org.scala-lang"
  val dottyGithubUrl = "https://github.com/scala/scala3"
  val dottyGithubRawUserContentUrl = "https://raw.githubusercontent.com/scala/scala3"

  // Run tests with filter through vulpix test suite
  val testCompilation = inputKey[Unit]("runs integration test with the supplied filter")

  // Used to compile files similar to ./bin/scalac script
  val scalac = inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

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
      //"-Werror",
      //"-Wunused:all",
      "-encoding", "UTF8",
      "-language:implicitConversions",
      s"--java-output-version:${Versions.minimumJVMVersion}",
      "-Yexplicit-nulls",
      "-Wsafe-init"
    ),

    (Compile / compile / javacOptions) ++= Seq(
      "-Xlint:unchecked",
      "-Xlint:deprecation",
      "--release", Versions.minimumJVMVersion
    ),

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
            .withLocal(buildCache.local.withEnabled(false).withStoreEnabled(false))
            .withRemote(buildCache.remote.withEnabled(false).withStoreEnabled(false))
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
    javaOptions ++= "-XX:MaxJavaStackTraceDepth=1000000" :: Nil,

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

  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  private val shellBanner: String =
    """%n      ________ ___   / /  ___
       |%n    / __/ __// _ | / /  / _ |
       |%n  __\\ \\/ /__/ __ |/ /__/ __ |
       |%n /____/\\___/_/ |_/____/_/ | |
       |%n                          |/  %s""".stripMargin.replace("\n", "")

  // Common generator for properties files
  lazy val generatePropertiesFile = (fileName: String, contents: Def.Initialize[String]) => Def.task {
    val file = (Compile / resourceManaged).value / fileName
    val data = contents.value
    if (!(file.exists && IO.read(file) == data)) {
      IO.write(file, data)
    }
    Seq(file)
  }

  // Generate compiler.properties consumed by sbt
  lazy val generateCompilerProperties: Def.Initialize[Task[Seq[File]]] = {
    import java.util._
    import java.text._
    val dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss")
    dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"))

    val fileName = "compiler.properties"
    val contents = Def.setting {
      s"""version.number=${version.value}
          |maven.version.number=${version.value}
          |git.hash=${VersionUtil.gitHash}
          |copyright.string=Copyright 2002-$currentYear, LAMP/EPFL
          |""".stripMargin
    }
    generatePropertiesFile(fileName, contents)
  }

  // Generate library.properties consumed by scala.util.Properties
  lazy val generateLibraryProperties: Def.Initialize[Task[Seq[File]]] = {
    val fileName = "library.properties"
    val contents = Def.setting {
      s"""version.number=${version.value}
          |maven.version.number=${version.value}
          |copyright.string=Copyright 2002-$currentYear, LAMP/EPFL
          |shell.banner=${shellBanner}
          |""".stripMargin
    }
    generatePropertiesFile(fileName, contents)
  }

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

  // Creates a scalaInstance by fetching the compiler from Maven.
  // Used by non-bootstrapped projects that need a published compiler.
  def fetchedScalaInstanceSettings = Def.settings(
    // sbt adds all the projects to scala-tool config which breaks building the scalaInstance
    // as a workaround, we build it manually by only adding the compiler
    scalaInstance := {
      val lm = dependencyResolution.value
      val log = streams.value.log
      val ver = scalaVersion.value
      val retrieveDir = streams.value.cacheDirectory / "scala3-compiler" / ver
      val comp = lm.retrieve("org.scala-lang" % "scala3-compiler_3" %
        ver, scalaModuleInfo = None, retrieveDir, log)
        .fold(w => throw w.resolveException, identity)
      Defaults.makeScalaInstance(
        ver,
        Array.empty,
        comp.toSeq,
        Seq.empty,
        state.value,
        scalaInstanceTopLoader.value,
      )
    },
  )

  // Common scalaInstance settings for bootstrapped projects compiled with the non-bootstrapped compiler.
  lazy val bootstrappedScalaInstanceSettings = Def.settings(
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
  ) ++ scaladocDerivedInstanceSettings

  // Setups up doc / scalaInstance to use in the bootstrapped projects instead of the default one
  lazy val scaladocDerivedInstanceSettings = Def.settings(
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
  )

  // Settings used when compiling dotty with a non-bootstrapped dotty
  lazy val commonBootstrappedSettings = commonDottySettings ++ Seq(
    // To enable support of scaladoc and language-server projects you need to change this to true
    bspEnabled := false,
    (Compile / unmanagedSourceDirectories) += baseDirectory.value / "src-bootstrapped",

    version := dottyVersion,
    scalaVersion := dottyNonBootstrappedVersion,

    scalaCompilerBridgeBinaryJar := {
      Some((`scala3-sbt-bridge-nonbootstrapped` / Compile / packageBin).value)
    },

    // Use the same name as the non-bootstrapped projects for the artifacts.
    // Remove the `js` suffix because JS artifacts are published using their special crossVersion.
    // The order of the two `stripSuffix`es is important, so that
    // scala3-library-bootstrappedjs becomes scala3-library.
    moduleName ~= { _.stripSuffix("js").stripSuffix("-bootstrapped") },

    // sbt gets very unhappy if two projects use the same target
    target := baseDirectory.value / ".." / "out" / "bootstrap" / name.value,

    // Compile using the non-bootstrapped and non-published dotty
    managedScalaInstance := false,
    scalaInstance := {
      val externalCompilerDeps = (`scala3-compiler-nonbootstrapped` / Compile / externalDependencyClasspath).value.map(_.data).toSet

      // IMPORTANT: We need to use actual jars to form the ScalaInstance and not
      // just directories containing classfiles because sbt maintains a cache of
      // compiler instances. This cache is invalidated based on timestamps
      // however this is only implemented on jars, directories are never
      // invalidated.
      val tastyCore = (`tasty-core-nonbootstrapped` / Compile / packageBin).value
      val scala3Library = (`scala3-library-nonbootstrapped` / Compile / packageBin).value
      val scalaLibrary = (`scala-library-nonbootstrapped` / Compile / packageBin).value
      val scala3Interfaces = (`scala3-interfaces` / Compile / packageBin).value
      val scala3Compiler = (`scala3-compiler-nonbootstrapped` / Compile / packageBin).value

      val libraryJars = Array(scala3Library, scalaLibrary)
      val compilerJars = Seq(tastyCore, scala3Interfaces, scala3Compiler) ++ externalCompilerDeps

      Defaults.makeScalaInstance(
        scalaVersion.value,
        libraryJars = libraryJars,
        allCompilerJars = compilerJars,
        allDocJars = Seq.empty,
        state.value,
        scalaInstanceTopLoader.value
      )
    },
    // force recompilation of bootstrapped modules when the compiler changes
    Compile / extraDevelocityCacheInputFiles ++=
      (`scala3-compiler-nonbootstrapped` / Compile / fullClasspathAsJars).value.map(_.data.toPath)
  ) ++ scaladocDerivedInstanceSettings

  /*lazy val commonBenchmarkSettings = Seq(
    Jmh / bspEnabled := false,
    Jmh / run / mainClass := Some("dotty.tools.benchmarks.Bench"), // custom main for jmh:run
    javaOptions += "-DBENCH_COMPILER_CLASS_PATH=" + Attributed.data((`scala3-bootstrapped` / Compile / fullClasspath).value).mkString("", File.pathSeparator, ""),
    javaOptions += "-DBENCH_CLASS_PATH=" + Attributed.data((`scala3-library-bootstrapped` / Compile / fullClasspath).value).mkString("", File.pathSeparator, "")
  )*/

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

  def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + File.pathSeparator + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }

  // ==============================================================================================
  // ================================= NON-BOOTSTRAPPED PROJECTS ==================================
  // ==============================================================================================

  lazy val `scala3-nonbootstrapped` = project.in(file("."))
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
      }.evaluated,
      bspEnabled := false,
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
      fetchedScalaInstanceSettings,
    )

  // ==============================================================================================
  // =================================== BOOTSTRAPPED PROJECTS ====================================
  // ==============================================================================================

  lazy val `scala3-bootstrapped` = project
    .enablePlugins(ScriptedPlugin)
    .aggregate(`scala3-interfaces`, `scala3-library-bootstrapped` , `scala-library-bootstrapped`,
      `tasty-core-bootstrapped`, `scala3-compiler-bootstrapped`, `scala3-sbt-bridge-bootstrapped`,
      `scala3-staging`, `scala3-tasty-inspector`, `scala-library-sjs`, `scala3-library-sjs`,
      scaladoc, `scala3-repl`, `scala3-presentation-compiler`, `scala3-language-server`)
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
      target := (ThisBuild / baseDirectory).value / "target" / "scala3-bootstrapped",
      scalac := Def.inputTaskDyn {
        val log = streams.value.log
        val externalDeps = (`scala3-compiler-bootstrapped` / Runtime / externalDependencyClasspath).value
        val stdlib = (`scala-library-bootstrapped` / Compile / packageBin).value.getAbsolutePath.toString
        val dottyCompiler = (`scala3-compiler-bootstrapped` / Compile / packageBin).value.getAbsolutePath.toString
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
          val dottyStaging = (`scala3-staging` / Compile / packageBin).value.getAbsolutePath.toString
          val dottyTastyInspector = (`scala3-tasty-inspector` / Compile / packageBin).value.getAbsolutePath.toString
          val tastyCore = (`tasty-core-bootstrapped` / Compile / packageBin).value.getAbsolutePath.toString
          val asm = findArtifactPath(externalDeps, "scala-asm")
          val compilerInterface = findArtifactPath(externalDeps, "compiler-interface")
          extraClasspath ++= Seq(dottyCompiler, dottyInterfaces, asm, dottyStaging, dottyTastyInspector, tastyCore, compilerInterface)
        }

        val wrappedArgs = if (printTasty) args else insertClasspathInArgs(args, extraClasspath.mkString(File.pathSeparator))
        val fullArgs = main :: (defaultOutputDirectory ::: wrappedArgs).map("\""+ _ + "\"").map(_.replace("\\", "\\\\"))

        (`scala3-compiler-bootstrapped` / Compile / runMain).toTask(fullArgs.mkString(" ", " ", ""))
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
          (`scala3-compiler-bootstrapped` / Test / testOnly).toTask(" not.a.test")
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
          (`scala3-compiler-bootstrapped` / Test / testOnly).toTask(cmd)
        }
      }.evaluated,
      // ================================ SBT SCRIPT TEST SETTINGS ================================
      sbtTestDirectory := (ThisBuild / baseDirectory).value / "sbt-test",
      // The batch mode accidentally became the default with no way to disable
      // it in sbt 1.4 (https://github.com/sbt/sbt/issues/5913#issuecomment-716003195).
      // We enable it explicitly here to make it clear that we're using it.
      scriptedBatchExecution := true,
      scriptedLaunchOpts ++= Seq(
        s"-Dplugin.scalaVersion=${dottyVersion}",
        s"-Dplugin.scala2Version=${ScalaLibraryPlugin.scala2Version}",
        s"-Dplugin.scalaJSVersion=${scalaJSVersion}",
      ),
      scriptedBufferLog := true,
      scripted := scripted.dependsOn(
        (`scala3-sbt-bridge-bootstrapped` / publishLocalBin),
        (`scala3-interfaces` / publishLocalBin),
        (`scala3-compiler-bootstrapped` / publishLocalBin),
        (`scala3-library-bootstrapped` / publishLocalBin),
        (`scala-library-bootstrapped` / publishLocalBin),
        (`scala-library-sjs` / publishLocalBin),
        (`scala3-library-sjs` / publishLocalBin),
        (`tasty-core-bootstrapped` / publishLocalBin),
        (`scala3-staging` / publishLocalBin),
        (`scala3-tasty-inspector` / publishLocalBin),
        (scaladoc / publishLocalBin),
        (`scala3-repl` / publishLocalBin),
        publishLocalBin,
      ).evaluated,
      bspEnabled := false,
    )

  /* Configuration of the org.scala-lang:scala3-sbt-bridge:*.**.**-bootstrapped project */
  lazy val `scala3-sbt-bridge-bootstrapped` = project.in(file("sbt-bridge"))
    .dependsOn(`scala3-compiler-bootstrapped`, `scala3-repl`) // TODO: Would this actually evict the reference compiler in scala-tool?
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
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src-bootstrapped",
      Compile / resourceDirectory := baseDirectory.value / "resources",
      // Add all the project's external dependencies
      libraryDependencies ++= Seq(
        ("org.scala-sbt" %% "zinc-apiinfo" % "1.8.0" % Test).cross(CrossVersion.for3Use2_13),
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
        ),
      // Packaging configuration of `scala3-sbt-bridge`
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := true,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // non-bootstrapped stdlib is publishable (only locally)
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-sbt-bridge-bootstrapped",
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      bspEnabled := false,
    )

  /* Configuration of the org.scala-lang:scala3-staging:*.**.**-bootstrapped project */
  lazy val `scala3-staging` = project.in(file("staging"))
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-staging (see sbt-test/sbt-dotty/quoted-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    .dependsOn(`scala3-compiler-bootstrapped` % "provided; compile->runtime; test->test")
    .settings(publishSettings)
    .settings(
      name          := "scala3-staging",
      moduleName    := "scala3-staging",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := true, // org.scala-lang:scala3-staging has a crosspath
      autoScalaLibrary := false, // do not add a dependency to stdlib, we depend transitively on the stdlib from `scala3-compiler-bootstrapped`
      // Add the source directories for the sbt-bridge (boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      // Packaging configuration of `scala3-staging`
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := true,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      publish / skip := false,
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      bspEnabled := false,
    )

  /* Configuration of the org.scala-lang:scala3-tasty-inspector:*.**.**-bootstrapped project */
  lazy val `scala3-tasty-inspector` = project.in(file("tasty-inspector"))
    // We want the compiler to be present in the compiler classpath when compiling this project but not
    // when compiling a project that depends on scala3-tasty-inspector (see sbt-test/sbt-dotty/tasty-inspector-example-project),
    // but we always need it to be present on the JVM classpath at runtime.
    .dependsOn(`scala3-compiler-bootstrapped` % "provided; compile->runtime; test->test")
    .settings(publishSettings)
    .settings(
      name          := "scala3-tasty-inspector",
      moduleName    := "scala3-tasty-inspector",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := true, // org.scala-lang:scala3-tasty-inspector has a crosspath
      autoScalaLibrary := false, // do not add a dependency to stdlib, we depend transitively on the stdlib from `scala3-compiler-bootstrapped`
      // Add the source directories for the sbt-bridge (boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      // Make sure that the produced artifacts have the minimum JVM version in the bytecode
      // Packaging configuration of `scala3-staging`
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := true,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      publish / skip := false,
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      bspEnabled := false,
    )

  lazy val `scala3-repl` = project.in(file("repl"))
    .dependsOn(`scala3-compiler-bootstrapped` % "compile->compile;test->test")
    .settings(publishSettings)
    .settings(
      name          := "scala3-repl",
      moduleName    := "scala3-repl",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := true,
      autoScalaLibrary := false,
      // Add the source directories for the sbt-bridge (boostrapped)
      Compile / unmanagedSourceDirectories   := Seq(baseDirectory.value / "src"),
      Compile / unmanagedResourceDirectories := Seq(baseDirectory.value / "resources"),
      Test    / unmanagedSourceDirectories   := Seq(baseDirectory.value / "test"),
      Test    / unmanagedResourceDirectories := Seq(baseDirectory.value / "test-resources"),
      // Packaging configuration of `scala3-staging`
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := true,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      publish / skip := false,
      libraryDependencies ++= Seq(
        "org.jline" % "jline-reader" % "3.29.0",
        "org.jline" % "jline-terminal" % "3.29.0",
        "org.jline" % "jline-terminal-jni" % "3.29.0",
        "com.lihaoyi" %% "pprint"     % "0.9.3",
        "com.lihaoyi" %% "fansi"      % "0.5.1",
        "com.lihaoyi" %% "sourcecode" % "0.4.4",
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
        "io.get-coursier" % "interface" % "1.0.28", // used by the REPL for dependency resolution
        "org.virtuslab" % "using_directives" % "1.1.4", // used by the REPL for parsing magic comments
      ),
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
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
          s"-Ddotty.tests.classes.dottyCompiler=${(`scala3-compiler-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.dottyRepl=${(ThisProject / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.tastyCore=${(`tasty-core-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.compilerInterface=${findArtifactPath(externalDeps, "compiler-interface")}",
          s"-Ddotty.tests.classes.scalaLibrary=${(`scala-library-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.scalaJSScalalib=${(`scala-library-sjs` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.scalaAsm=${findArtifactPath(externalDeps, "scala-asm")}",
          s"-Ddotty.tests.classes.jlineTerminal=${findArtifactPath(externalDeps, "jline-terminal")}",
          s"-Ddotty.tests.classes.jlineReader=${findArtifactPath(externalDeps, "jline-reader")}",
          s"-Ddotty.tests.classes.dottyStaging=${(LocalProject("scala3-staging") / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.dottyTastyInspector=${(LocalProject("scala3-tasty-inspector") / Compile / packageBin).value}",
          s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
        )
      },
      run / fork := true,
      excludeDependencies += "org.scala-lang" %% "scala3-library",
      excludeDependencies += "org.scala-lang" % "scala-library",
      Compile / run := {
        //val classpath = s"-classpath ${(`scala-library-bootstrapped` / Compile / packageBin).value}"
        // TODO: We should use the val above instead of `-usejavacp` below. SBT crashes we we have a val and we call toTask
        // with it as a parameter. THIS IS NOT A LEGIT USE CASE OF THE `-usejavacp` FLAG.
        (Compile / run).partialInput(" -usejavacp").evaluated
      },
      bspEnabled := false,
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
      Compile / compile / scalacOptions ++= Seq(
        // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called
        "-sourcepath", (Compile / sourceDirectories).value.map(_.getCanonicalPath).distinct.mkString(File.pathSeparator),
      ),
      // Packaging configuration of the stdlib
      Compile / publishArtifact := true,
      Test    / publishArtifact := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala-library-nonbootstrapped",
      // Add configuration for MiMa
      commonMiMaSettings,
      mimaForwardIssueFilters := MiMaFilters.Scala3Library.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.Scala3Library.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.Scala3Library"),
      // Should we also patch .sjsir files
      keepSJSIR := false,
      // Generate library.properties, used by scala.util.Properties
      Compile / resourceGenerators += generateLibraryProperties.taskValue,
      Compile / mainClass := None,
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
      fetchedScalaInstanceSettings,
      // This Project only has a dependency to `org.scala-lang:scala-library:*.**.**-nonbootstrapped`
      emptyPublishedJarSettings,  // Validate JAR is empty (only META-INF)
      // Packaging configuration of the stdlib
      Compile / publishArtifact := true,
      Test    / publishArtifact := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-library-nonbootstrapped",
      Compile / mainClass := None,
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
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := false, // org.scala-lang:scala-library doesn't have a crosspath
      autoScalaLibrary     := false, // DO NOT DEPEND ON THE STDLIB, IT IS THE STDLIB
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src-bootstrapped",
      Compile / compile / scalacOptions ++= Seq(
        // Needed so that the library sources are visible when `dotty.tools.dotc.core.Definitions#init` is called
        "-sourcepath", (Compile / sourceDirectories).value.map(_.getCanonicalPath).distinct.mkString(File.pathSeparator),
      ),
      // Packaging configuration of the stdlib
      Compile / publishArtifact := true,
      Test    / publishArtifact := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala-library-bootstrapped",
      // Configure the nonbootstrapped compiler
      bootstrappedScalaInstanceSettings,
      // Add configuration for MiMa
      commonMiMaSettings,
      mimaForwardIssueFilters := MiMaFilters.Scala3Library.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.Scala3Library.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.Scala3Library"),
      // Should we also patch .sjsir files
      keepSJSIR := false,
      // Generate Scala 3 runtime properties overlay
      Compile / resourceGenerators += generateLibraryProperties.taskValue,
      bspEnabled := enableBspAllProjects,
      Compile / mainClass := None,
    )

  /* Configuration of the org.scala-lang:scala3-library_3:*.**.**-bootstrapped project */
  lazy val `scala3-library-bootstrapped` = project.in(file("library"))
    .dependsOn(`scala-library-bootstrapped`)
    .settings(publishSettings)
    .settings(
      name          := "scala3-library-bootstrapped",
      moduleName    := "scala3-library",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      // sbt defaults to scala 2.12.x and metals will report issues as it doesn't consider the project a scala 3 project
      // (not the actual version we use to compile the project)
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := true, // org.scala-lang:scala3-library has a crosspath
      // Do not depend on the `org.scala-lang:scala3-library` automatically, we manually depend on `scala-library-bootstrapped`
      autoScalaLibrary := false,
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      // This Project only has a dependency to `org.scala-lang:scala-library:*.**.**-bootstrapped`
      emptyPublishedJarSettings,  // Validate JAR is empty (only META-INF)
      // Packaging configuration of the stdlib
      Compile / publishArtifact := true,
      Test    / publishArtifact := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-library-bootstrapped",
      bspEnabled := enableBspAllProjects,
      Compile / mainClass := None,
    )

  /* Configuration of the org.scala-js:scalajs-scalalib_2.13:*.**.**-bootstrapped project */
  lazy val `scala-library-sjs` = project.in(file("library-js"))
    // We add a dependency to the JVM library to have the classfile available
    // (as they are not part of this artifact)
    .dependsOn(`scala3-library-bootstrapped`)
    .enablePlugins(ScalaLibraryPlugin, DottyJSPlugin)
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
      crossVersion  := CrossVersion.disabled,
      // sbt defaults to scala 2.12.x and metals will report issues as it doesn't consider the project a scala 3 project
      // (not the actual version we use to compile the project)
      scalaVersion  := dottyNonBootstrappedVersion,
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Compile / unmanagedSourceDirectories ++=
        (`scala-library-bootstrapped` / Compile / unmanagedSourceDirectories).value,
      // Configure the source maps to point to GitHub for releases
      Compile / compile / scalacOptions ++= {
        if (isRelease) {
          val baseURI = (LocalRootProject / baseDirectory).value.toURI
          val dottyVersion = version.value
          Seq(s"-scalajs-mapSourceURI:$baseURI->$dottyGithubRawUserContentUrl/$dottyVersion/")
        } else {
          Nil
        }
      },
      // Packaging configuration of the stdlib
      Compile / publishArtifact := true,
      Test    / publishArtifact := false,
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

        files.filterNot(_.getPath().contains("BoxesRunTime.scala"))
             .filterNot(_.getPath().contains("ScalaNumber.scala"))
             .filterNot(file =>
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
      autoScalaLibrary := false,
      // Configure the nonbootstrapped compiler
      bootstrappedScalaInstanceSettings,
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
      // Add configuration for MiMa
      commonMiMaSettings,
      mimaForwardIssueFilters := MiMaFilters.ScalaLibrarySJS.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.ScalaLibrarySJS.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.ScalaLibrarySJS"),
      // Should we also patch .sjsir files
      keepSJSIR := true,
      bspEnabled := false,
      Compile / mainClass := None,
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
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := true, // org.scala-lang:scala3-library_sjs1 has a crosspath
      // Do not depend on the `org.scala-lang:scala3-library` automatically, we manually depend on `scala-library-bootstrapped`
      autoScalaLibrary := false,
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      // This Project only has a dependency to `org.scala-js:scalajs-scalalib:*.**.**-bootstrapped`
      emptyPublishedJarSettings,  // Validate JAR is empty (only META-INF)
      // Packaging configuration of the stdlib
      Compile / publishArtifact := true,
      Test    / publishArtifact := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-library",
      bspEnabled := false,
      Compile / mainClass := None,
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
      fetchedScalaInstanceSettings,
      // Add configuration of the test
      Test / envVars ++= Map(
        "EXPECTED_TASTY_VERSION" -> expectedTastyVersion,
      ),
      mimaForwardIssueFilters := MiMaFilters.TastyCore.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.TastyCore.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.TastyCore"),
    )

  /* Configuration of the org.scala-lang:tasty-core_3:*.**.**-bootstrapped project */
  lazy val `tasty-core-bootstrapped` = project.in(file("tasty"))
    .dependsOn(`scala3-library-bootstrapped`)
    .settings(publishSettings)
    .settings(commonMiMaSettings)
    .settings(
      name          := "tasty-core-bootstrapped",
      moduleName    := "tasty-core",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion,
      crossPaths    := true, // org.scala-lang:tasty-core has a crosspath
      // sbt shouldn't add stdlib automatically, we depend on `scala3-library-nonbootstrapped`
      autoScalaLibrary := false,
      // Add the source directories for the stdlib (non-boostrapped)
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      Compile / unmanagedSourceDirectories += baseDirectory.value / "src-bootstrapped",
      // Add all the project's external dependencies
      libraryDependencies ++= Seq(
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      ),
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := true,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "tasty-core-bootstrapped",
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      // Add configuration of the test
      Test / envVars ++= Map(
        "EXPECTED_TASTY_VERSION" -> expectedTastyVersion,
      ),
      mimaForwardIssueFilters := MiMaFilters.TastyCore.ForwardsBreakingChanges,
      mimaBackwardIssueFilters := MiMaFilters.TastyCore.BackwardsBreakingChanges,
      customMimaReportBinaryIssues("MiMaFilters.TastyCore"),
      bspEnabled := false,
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
      scalaVersion  := referenceVersion, // nonbootstrapped artifacts are compiled with the reference compiler (already officially published)
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
        "org.scala-lang.modules" % "scala-asm" % "9.9.0-scala-1",
        Dependencies.compilerInterface,
        ("io.get-coursier" %% "coursier" % "2.0.16" % Test).cross(CrossVersion.for3Use2_13),
      ),
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
      Compile / resourceGenerators += generateCompilerProperties.taskValue,
      // sbt adds all the projects to scala-tool config which breaks building the scalaInstance
      // as a workaround, I build it manually by only adding the compiler
      managedScalaInstance := false,
      fetchedScalaInstanceSettings,
      scalaCompilerBridgeBinaryJar := {
        val lm = dependencyResolution.value
        val log = streams.value.log
        val version = scalaVersion.value
        val retrieveDir = streams.value.cacheDirectory / "scala3-sbt-bridge" / version
        val comp = lm.retrieve("org.scala-lang" % "scala3-sbt-bridge" %
          version, scalaModuleInfo = None, retrieveDir, log)
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
          s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
        )
      },
    )

  /* Configuration of the org.scala-lang:scala3-compiler_3:*.**.**-bootstrapped project */
  lazy val `scala3-compiler-bootstrapped` = project.in(file("compiler"))
    .dependsOn(`scala3-interfaces`, `tasty-core-bootstrapped`, `scala3-library-bootstrapped`)
    .settings(publishSettings)
    .settings(
      name          := "scala3-compiler-bootstrapped",
      moduleName    := "scala3-compiler",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion,
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
        "org.scala-lang.modules" % "scala-asm" % "9.9.0-scala-1",
        Dependencies.compilerInterface,
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
        ("io.get-coursier" %% "coursier" % "2.0.16" % Test).cross(CrossVersion.for3Use2_13),
      ),
      // Specify the default entry point of the compiler
      Compile / mainClass := Some("dotty.tools.dotc.Main"),
      // Add entry's to the MANIFEST
      packageOptions += ManifestAttributes(("Git-Hash", VersionUtil.gitHash)), // Used by the REPL
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := true,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Project specific target folder. sbt doesn't like having two projects using the same target folder
      target := target.value / "scala3-compiler-bootstrapped",
      // Generate compiler.properties, used by sbt
      Compile / resourceGenerators += generateCompilerProperties.taskValue,
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
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
          s"-Ddotty.tests.classes.tastyCore=${(`tasty-core-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.compilerInterface=${findArtifactPath(externalDeps, "compiler-interface")}",
          s"-Ddotty.tests.classes.scalaLibrary=${(`scala-library-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.scalaJSScalalib=${(`scala-library-sjs` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.scalaAsm=${findArtifactPath(externalDeps, "scala-asm")}",
          s"-Ddotty.tests.classes.dottyStaging=${(LocalProject("scala3-staging") / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.dottyTastyInspector=${(LocalProject("scala3-tasty-inspector") / Compile / packageBin).value}",
          s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
        )
      },
      bspEnabled := enableBspAllProjects,
    )

  // ==============================================================================================
  // ========================================== SCALADOC ==========================================
  // ==============================================================================================
  val SourceLinksIntegrationTest = config("sourceLinksIntegrationTest") extend Test

  /* Configuration of the org.scala-lang:scaladoc_3:*.**.**-bootstrapped project */
  lazy val scaladoc = project.in(file("scaladoc"))
    .dependsOn(`scala3-compiler-bootstrapped`, `scala3-tasty-inspector`)
    .settings(publishSettings)
    .settings(
      name          := "scaladoc",
      moduleName    := "scaladoc",
      version       := dottyVersion,
      versionScheme := Some("semver-spec"),
      scalaVersion  := dottyNonBootstrappedVersion,
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
      Test / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
      // All the dependencies needed by the doctool
      libraryDependencies ++= Dependencies.flexmarkDeps ++ Seq(
        "nl.big-o" % "liqp" % "0.8.2",
        "org.jsoup" % "jsoup" % "1.17.2", // Needed to process .html files for static site
        Dependencies.`jackson-dataformat-yaml`,
        "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      ),
      Compile / scalacOptions += "-experimental",
      // Packaging configuration of the stdlib
      Compile / packageBin / publishArtifact := true,
      Compile / packageDoc / publishArtifact := true,
      Compile / packageSrc / publishArtifact := true,
      // Only publish compilation artifacts, no test artifacts
      Test    / publishArtifact := false,
      // Do not allow to publish this project for now
      publish / skip := false,
      // Main class configuration
      Compile / mainClass := Some("dotty.tools.scaladoc.Main"),
      run / baseDirectory := (ThisBuild / baseDirectory).value,
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      bspEnabled := enableBspAllProjects,
    )
    // Build information configuration
    .settings(
      Compile / buildInfoKeys := Seq[BuildInfoKey](version),
      Compile / buildInfoPackage := "dotty.tools.scaladoc",
      Test / buildInfoPackage := "dotty.tools.scaladoc.test",
      Test / buildInfoKeys := Seq[BuildInfoKey](
        (Test / Build.testcasesOutputDir),
        (Test / Build.testcasesSourceRoot),
        Build.testDocumentationRoot,
      ),
      BuildInfoPlugin.buildInfoScopedSettings(Compile),
      BuildInfoPlugin.buildInfoScopedSettings(Test),
      BuildInfoPlugin.buildInfoDefaultSettings,
    )
    // Test configuration
    .settings(
      Test / test := (Test / test).dependsOn(`scaladoc-testcases` / Compile / compile).value,
      Test / testcasesOutputDir := (`scaladoc-testcases` / Compile / products).value.map(_.getAbsolutePath),
      Test / testcasesSourceRoot := ((`scaladoc-testcases` / baseDirectory).value / "src").getAbsolutePath.toString,
      testDocumentationRoot := (baseDirectory.value / "test-documentations").getAbsolutePath,
    )
    // Test configuration for source links integration test
    .configs(SourceLinksIntegrationTest)
    .settings(
      inConfig(SourceLinksIntegrationTest)(Defaults.testSettings),
      SourceLinksIntegrationTest / scalaSource := baseDirectory.value / "test-source-links",
      SourceLinksIntegrationTest / test := ((SourceLinksIntegrationTest / test).dependsOn(generateScalaDocumentation.toTask(""))).value,
    )
      // Documentation generation tasks
    .settings(
      generateSelfDocumentation := Def.taskDyn {
        generateDocumentation(Scaladoc)
      }.value,

      generateScalaDocumentation := Def.inputTaskDyn {
        val majorVersion = (`scala-library-bootstrapped` / scalaBinaryVersion).value
        val extraArgs = spaceDelimited("[<output-dir>] [--justAPI]").parsed
        val outputDirOverride = extraArgs.headOption.fold(identity[GenerationConfig](_))(newDir => {
          config: GenerationConfig => config.add(OutputDir(newDir))
        })
        val justAPI = extraArgs.contains("--justAPI")
        def justAPIOverride(config: GenerationConfig): GenerationConfig = {
          if (!justAPI) config
          else {
            val siteRoot = IO.createTemporaryDirectory.getAbsolutePath()
            config.add(SiteRoot(siteRoot))
          }
        }
        // It would be the easiest to create a temp directory and apply patches there, but this task would be used frequently during development
        // If we'd build using copy the emitted warnings would point developers to copies instead of original sources. Any fixes made in there would be lost.
        // Instead let's apply revertable patches to the files as part snapshot doc generation process
        abstract class SourcePatch(val file: File) {
          def apply(): Unit
          def revert(): Unit
        }
        val docs = file("docs")
        val sourcePatches = if (justAPI) Nil else Seq(
          // Generate full sidebar.yml based on template and reference content
          new SourcePatch(docs / "sidebar.yml") {
            val referenceSideBarCopy = IO.temporaryDirectory / "sidebar.yml.copy"
            IO.copyFile(file, referenceSideBarCopy)

            override def apply(): Unit = {
              val yaml = new org.yaml.snakeyaml.Yaml()
              type YamlObject = java.util.Map[String, AnyRef]
              type YamlList[T] = java.util.List[T]
              def loadYaml(file: File): YamlObject = {
                val reader = Files.newBufferedReader(file.toPath)
                try yaml.load(reader).asInstanceOf[YamlObject]
                finally reader.close()
              }
              // Ensure to always operate on original (Map, List) instances
              val template = loadYaml(docs / "sidebar.nightly.template.yml")
              template.get("subsection")
                .asInstanceOf[YamlList[YamlObject]]
                .stream()
                .filter(_.get("title") == "Reference")
                .findFirst()
                .orElseThrow(() => new IllegalStateException("Reference subsection not found in sidebar.nightly.template.yml"))
                .putAll(loadYaml(referenceSideBarCopy))

              val sidebarWriter = Files.newBufferedWriter(this.file.toPath)
              try yaml.dump(template, sidebarWriter)
              finally sidebarWriter.close()
            }
            override def revert(): Unit = IO.move(referenceSideBarCopy, file)
          },
          // Add patch about nightly version usage
          new SourcePatch(docs / "_layouts" / "static-site-main.html") {
            lazy val originalContent = IO.read(file)

            val warningMessage = """{% if page.nightlyOf %}
              |  <aside class="warning">
              |    <div class='icon'></div>
              |    <div class='content'>
              |      This is a nightly documentation. The content of this page may not be consistent with the current stable version of language.
              |      Click <a href="{{ page.nightlyOf }}">here</a> to find the stable version of this page.
              |    </div>
              |  </aside>
              |{% endif %}""".stripMargin

            override def apply(): Unit = {
              IO.write(file,
                originalContent
                .replace("{{ content }}", s"$warningMessage {{ content }}")
                .ensuring(_.contains(warningMessage), "patch to static-site-main layout not applied!")
              )
            }
            override def revert(): Unit = IO.write(file, originalContent)
          }
        )
        val config = Def.task {
          outputDirOverride
          .andThen(justAPIOverride)
          .apply(Scala3.value)
        }
        val writeAdditionalFiles = Def.task {
          val dest = file(config.value.get[OutputDir].get.value)
          if (!justAPI) {
            IO.write(dest / "versions" / "latest-nightly-base", majorVersion)
            // This file is used by GitHub Pages when the page is available in a custom domain
            IO.write(dest / "CNAME", "dotty.epfl.ch")
          }
        }
        val applyPatches = Def.task {
          streams.value.log.info(s"Generating snapshot scaladoc, would apply patches to ${sourcePatches.map(_.file)}")
          sourcePatches.foreach(_.apply())
        }
        val revertPatches = Def.task {
          streams.value.log.info(s"Generated snapshot scaladoc, reverting changes made to ${sourcePatches.map(_.file)}")
          sourcePatches.foreach(_.revert())
        }
        writeAdditionalFiles.dependsOn(
          revertPatches.dependsOn(
            generateDocumentation(config)
              .dependsOn(applyPatches)
          )
        )
      }.evaluated,

      generateStableScala3Documentation := Def.inputTaskDyn {
        val extraArgs = spaceDelimited("<version>").parsed
        val version = baseVersion
        // In the early days of scaladoc there was a practice to precompile artifacts of Scala 3 and generate docs using different version of scaladoc
        // It's no longer needed after its stablisation.
        // Allow to use explcit version check to detect using incorrect revision during release process
        extraArgs.headOption.foreach { explicitVersion =>
          assert(
            explicitVersion == version,
            s"Version of the build ($version) does not match the explicit verion ($explicitVersion)"
          )
        }

        val docs = IO.createTemporaryDirectory
        IO.copyDirectory(file("docs"), docs)
        IO.delete(docs / "_blog")

        val config = Def.task {
          Scala3.value
          .add(ProjectVersion(version))
          .add(Revision(version))
          .add(OutputDir(s"scaladoc/output/${version}"))
          .add(SiteRoot(docs.getAbsolutePath))
          .remove[ApiSubdirectory]
        }
        generateDocumentation(config)
      }.evaluated,

      generateTestcasesDocumentation := Def.taskDyn {
        generateDocumentation(Testcases)
      }.value,

      // Generate the Scala 3 reference documentation (published at https://docs.scala-lang.org/scala3/reference)
      generateReferenceDocumentation := Def.inputTaskDyn {
        val shouldRegenerateExpectedLinks = (Space ~> literal("--no-regenerate-expected-links")).?.parsed.isEmpty

        val _ = generateStaticAssetsTask.value

        // Move all the source files to a temporary directory and apply some changes specific to the reference documentation
        val docs = IO.createTemporaryDirectory
        IO.copyDirectory(file("docs"), docs)
        IO.delete(docs / "_blog")

        // Add redirections from previously supported URLs, for some pages
        for (name <- Seq("changed-features", "contextual", "dropped-features", "metaprogramming", "other-new-features")) {
          val path = docs / "_docs" / "reference" / name / s"${name}.md"
          val contentLines = IO.read(path).linesIterator.to[collection.mutable.ArrayBuffer]
          contentLines.insert(1, s"redirectFrom: /${name}.html") // Add redirection
          val newContent = contentLines.mkString("\n")
          IO.write(path, newContent)
        }

        val languageReferenceConfig = Def.task {
          Scala3.value
            .add(OutputDir("scaladoc/output/reference"))
            .add(SiteRoot(docs.getAbsolutePath))
            .add(ProjectName("Scala 3 Reference"))
            .add(ProjectVersion(baseVersion))
            .remove[VersionsDictionaryUrl]
            .add(SourceLinks(List(
              s"${docs.getParentFile().getAbsolutePath}=github://scala/scala3/language-reference-stable"
            )))
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

    )

  lazy val `scala3-presentation-compiler` = project.in(file("presentation-compiler"))
    .settings(commonBootstrappedSettings)
    .dependsOn(`scala3-compiler-bootstrapped`, `scala3-library-bootstrapped`, `scala3-presentation-compiler-testcases` % "test->test")
    .settings(presentationCompilerSettings)
    .settings(scala3PresentationCompilerBuildInfo)

  def scala3PresentationCompilerBuildInfo =
    Seq(
      ideTestsDependencyClasspath := {
        val testCasesLib = (`scala3-presentation-compiler-testcases` / Compile / classDirectory).value
        val dottyLib = (`scala-library-bootstrapped` / Compile / classDirectory).value
        testCasesLib :: dottyLib :: Nil
      },
      Compile / buildInfoPackage := "dotty.tools.pc.buildinfo",
      Compile / buildInfoKeys := Seq(scalaVersion),
      Test / buildInfoPackage := "dotty.tools.pc.tests.buildinfo",
      Test / buildInfoKeys := Seq(scalaVersion, ideTestsDependencyClasspath)
    ) ++ BuildInfoPlugin.buildInfoScopedSettings(Compile) ++
      BuildInfoPlugin.buildInfoScopedSettings(Test) ++
      BuildInfoPlugin.buildInfoDefaultSettings

  lazy val presentationCompilerSettings = {
    val mtagsVersion = "1.6.3"
    Seq(
      libraryDependencies ++= Seq(
        "org.lz4" % "lz4-java" % "1.8.0",
        "io.get-coursier" % "interface" % "1.0.18",
        "org.scalameta" % "mtags-interfaces" % mtagsVersion,
        "com.google.guava" % "guava" % "33.2.1-jre",
      ),
      libraryDependencies += ("org.scalameta" % s"mtags-shared_${ScalaLibraryPlugin.scala2Version}" % mtagsVersion % SourceDeps),
      ivyConfigurations += SourceDeps.hide,
      transitiveClassifiers := Seq("sources"),
      publishLocal := publishLocal.dependsOn( // It is best to publish all together. It is not rare to make changes in both compiler / presentation compiler and it can get misaligned
        `scala3-compiler-bootstrapped` / publishLocal,
        `scala3-library-bootstrapped` / publishLocal,
        `scala-library-bootstrapped` / publishLocal,
      ).value,
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
      bspEnabled := enableBspAllProjects,
    )
  }

  lazy val `scala3-presentation-compiler-testcases` = project.in(file("presentation-compiler-testcases"))
    .dependsOn(`scala3-compiler-bootstrapped`)
    .settings(
      commonBootstrappedSettings,
      bspEnabled := enableBspAllProjects,
    )

  lazy val `scala3-language-server` = project.in(file("language-server")).
    dependsOn(`scala3-compiler-bootstrapped`, `scala3-repl`).
    settings(commonBootstrappedSettings).
    settings(
      libraryDependencies ++= Seq(
        "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.6.0",
        Dependencies.`jackson-databind`
      ),
      // Work around https://github.com/eclipse/lsp4j/issues/295
      dependencyOverrides += "org.eclipse.xtend" % "org.eclipse.xtend.lib" % "2.16.0",
      // Exclude the dependency that is resolved transively, the stdlib
      // is a project dependency instead
      excludeDependencies += "org.scala-lang" %% "scala3-library",
      javaOptions := (`scala3-compiler-bootstrapped` / javaOptions).value,
      scalacOptions -= "-Yexplicit-nulls",
    ).
    settings(
      ideTestsCompilerVersion := (`scala3-compiler-nonbootstrapped` / version).value,
      ideTestsCompilerArguments := Seq(),
      ideTestsDependencyClasspath := {
        val scalaLib = (`scala-library-bootstrapped` / Compile / classDirectory).value
        scalaLib :: Nil
      },
      Test / buildInfoKeys := Seq[BuildInfoKey](
        ideTestsCompilerVersion,
        ideTestsCompilerArguments,
        ideTestsDependencyClasspath
      ),
      Test / buildInfoPackage := "dotty.tools.languageserver.util.server",
      BuildInfoPlugin.buildInfoScopedSettings(Test),
      BuildInfoPlugin.buildInfoDefaultSettings,
      bspEnabled := false,
    )

  /** Common settings for sjsSandbox and sjsJUnitTests */
  lazy val regularScalaJSProjectSettings: Seq[Setting[_]] = Def.settings(
    version       := dottyVersion,
    scalaVersion  := referenceVersion,
    crossPaths    := true,
    autoScalaLibrary := false, // do not add a dependency to stdlib, we depend on it with dependsOn
    // Add the source directories
    Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src"),
    Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test"),
    // Don't publish
    publish / skip := false,
    // Configure to use the non-bootstrapped compiler
    bootstrappedScalaInstanceSettings,
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
    dependsOn(`scala-library-sjs`).
    settings(
      regularScalaJSProjectSettings,
      // Required to run Scala.js tests.
      Test / fork := false,

      scalaJSUseMainModuleInitializer := true,
      bspEnabled := false,
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
    dependsOn(`scala-library-sjs`).
    settings(
      regularScalaJSProjectSettings,
      bspEnabled := false,
      scalacOptions --= Seq("-Werror", "-deprecation", "-Yexplicit-nulls"),

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

        val isWebAssembly = linkerConfig.experimentalUseWebAssembly

        if (linkerConfig.moduleKind != ModuleKind.NoModule && !linkerConfig.closureCompiler && !isWebAssembly)
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
        val hasAsyncAwait = linkerConfig.esFeatures.esVersion >= ESVersion.ES2017
        val isWebAssembly = linkerConfig.experimentalUseWebAssembly

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
          ++ conditionally(hasModules && !linkerConfig.closureCompiler && !isWebAssembly, "js/src/test/require-multi-modules")
          ++ conditionally(moduleKind == ModuleKind.ESModule, "js/src/test/require-dynamic-import")
          ++ conditionally(moduleKind == ModuleKind.ESModule, "js/src/test/require-esmodule")

          ++ conditionally(hasAsyncAwait, "js/src/test/require-async-await")
          ++ conditionally(hasAsyncAwait && isWebAssembly, "js/src/test/require-orphan-await")
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
    dependsOn(`scala3-compiler-bootstrapped` % "test->test").
    settings(
      (Compile / scalaSource)    := baseDirectory.value / "src",
      (Test / scalaSource)       := baseDirectory.value / "test",
      (Compile / javaSource)    := baseDirectory.value / "src",
      (Test / javaSource)       := baseDirectory.value / "test",
      (Compile / resourceDirectory)    := baseDirectory.value / "resources",
      (Test / resourceDirectory)       := baseDirectory.value / "test-resources",
      scalaVersion := (`scala3-compiler-bootstrapped` / scalaVersion).value,
      libraryDependencies ++= Seq(
        "org.scala-js" %% "scalajs-linker" % scalaJSVersion % Test cross CrossVersion.for3Use2_13,
        "org.scala-js" %% "scalajs-env-nodejs" % "1.3.0" % Test cross CrossVersion.for3Use2_13,
      ),

      // Change the baseDirectory when running the tests
      Test / baseDirectory := baseDirectory.value.getParentFile,

      javaOptions ++= (`scala3-compiler-bootstrapped` / javaOptions).value,
      javaOptions ++= {
        val externalJSDeps = (`scala-library-sjs` / Compile / externalDependencyClasspath).value

        val managedSrcDir = {
          // Populate the directory
          (`scala3-compiler-bootstrapped` / Compile / managedSources).value

          (`scala3-compiler-bootstrapped` / Compile / sourceManaged).value
        }

        val externalDeps = (`scala3-compiler-bootstrapped` / Runtime / externalDependencyClasspath).value

        Seq(
          s"-Ddotty.tests.dottyCompilerManagedSources=${managedSrcDir}",
          s"-Ddotty.tests.classes.dottyInterfaces=${(`scala3-interfaces` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.dottyCompiler=${(`scala3-compiler-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.tastyCore=${(`tasty-core-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.compilerInterface=${findArtifactPath(externalDeps, "compiler-interface")}",
          s"-Ddotty.tests.classes.scalaLibrary=${(`scala-library-bootstrapped` / Compile / packageBin).value}",
          s"-Ddotty.tests.classes.scalaAsm=${findArtifactPath(externalDeps, "scala-asm")}",
          s"-Ddotty.tools.dotc.semanticdb.test=${(ThisBuild / baseDirectory).value/"tests"/"semanticdb"}",
          "-Ddotty.tests.classes.scalaJSScalalib=" + (`scala-library-sjs` / Compile / packageBin).value,
          "-Ddotty.tests.classes.scalaJSJavalib=" + findArtifactPath(externalJSDeps, "scalajs-javalib"),
          "-Ddotty.tests.classes.scalaJSLibrary=" + findArtifactPath(externalJSDeps, "scalajs-library_2.13"),
        )
      },
      // Configure to use the non-bootstrapped compiler
      bootstrappedScalaInstanceSettings,
      Test / forkOptions := (Test / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
      bspEnabled := false,
    )

  //lazy val `scala3-bench` = project.in(file("bench")).asDottyBench(NonBootstrapped)
  //lazy val `scala3-bench-bootstrapped` = project.in(file("bench")).asDottyBench(Bootstrapped)
  //lazy val `scala3-bench-run` = project.in(file("bench-run")).asDottyBench(Bootstrapped)

  /*lazy val `scala3-bench-micro` = project.in(file("bench-micro"))
    .asDottyBench(Bootstrapped)
    .settings(Jmh / run / mainClass := Some("org.openjdk.jmh.Main"))*/

  val testcasesOutputDir = taskKey[Seq[String]]("Root directory where tests classes are generated")
  val testcasesSourceRoot = taskKey[String]("Root directory where tests sources are generated")
  val testDocumentationRoot = taskKey[String]("Root directory where tests documentation are stored")
  val generateSelfDocumentation = taskKey[Unit]("Generate example documentation")
  val generateTestcasesDocumentation  = taskKey[Unit]("Generate documentation for testcases, useful for debugging tests")

  // Published on https://dotty.epfl.ch/ by nightly builds
  // Contains additional internal/contributing docs
  val generateScalaDocumentation = inputKey[Unit]("Generate documentation for snapshot release")

  // Published on https://docs.scala-lang.org/api/all.html
  val generateStableScala3Documentation  = inputKey[Unit]("Generate documentation for stable release")

  // Published on https://docs.scala-lang.org/scala3/reference/
  // Does not produce API docs, contains additional redirects for improved stablity
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
    dependsOn(`scala3-library-sjs`).
    settings(
      commonBootstrappedSettings,
      libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "2.8.0"))

  lazy val `scaladoc-js-main` = project.in(file("scaladoc-js/main")).
    enablePlugins(DottyJSPlugin).
    dependsOn(`scaladoc-js-common`).
    settings(
      commonBootstrappedSettings,
      scalaJSUseMainModuleInitializer := true,
      Test / fork := false,
      scalacOptions -= "-Yexplicit-nulls",
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


  val prepareCommunityBuild = taskKey[Unit]("Publish local the compiler and the sbt plugin. Also store the versions of the published local artefacts in two files, community-build/{scala3-bootstrapped.version,sbt-injected-plugins}.")

  lazy val `community-build` = project.in(file("community-build"))
    .settings(commonSettings)
    .settings(
      scalaVersion := referenceVersion,
      prepareCommunityBuild := {
        (`scala3-sbt-bridge-bootstrapped` / publishLocalBin).value
        (`scala3-interfaces` / publishLocalBin).value
        (`tasty-core-bootstrapped` / publishLocalBin).value
        (`scala3-library-bootstrapped` / publishLocalBin).value
        (`scala-library-bootstrapped` / publishLocalBin).value
        (`scala3-tasty-inspector` / publishLocalBin).value
        (scaladoc / publishLocalBin).value
        (`scala3-repl` / publishLocalBin).value
        (`scala3-compiler-bootstrapped` / publishLocalBin).value
        (`scala-library-sjs` / publishLocalBin).value
        (`scala3-library-sjs` / publishLocalBin).value
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
      scalacOptions -= "-Yexplicit-nulls",
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
      val line = "info.scala.versionLine" -> versionLine
      id.withExtraAttributes(id.extraAttributes + line)
    },
    Test / publishArtifact := false,
    homepage := Some(url(homepageUrl)),
    licenses += (("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))),
    scmInfo := Some(ScmInfo(url(dottyGithubUrl), "scm:git:git@github.com:scala/scala3.git")),
    developers := List(
      Developer(
        id = "scala",
        name = "The Scala Team",
        email = "security@scala-lang.org",
        url = url(homepageUrl)
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
    republishCommandLibs += ("scala" -> List("scala3-interfaces", "scala3-compiler", "scala3-library", "scala-library", "tasty-core", "scala3-repl")),
    republishCommandLibs += ("with_compiler" -> List("scala3-staging", "scala3-tasty-inspector", "scala3-repl", "^!scala3-interfaces", "^!scala3-compiler", "^!scala3-library", "^!scala-library", "^!tasty-core")),
    republishCommandLibs += ("scaladoc" -> List("scala3-interfaces", "scala3-compiler", "scala3-library", "scala-library", "tasty-core", "scala3-tasty-inspector", "scaladoc")),
  )

  lazy val dist = project.asDist
    .settings(packageName := "scala3-" + dottyVersion)
    .settings(
      republishLibexecDir := baseDirectory.value / "libexec",
      republishCoursier +=
        ("coursier.jar" -> s"https://github.com/coursier/coursier/releases/download/v$coursierJarVersion/coursier.jar"),
      republishLaunchers +=
        ("scala-cli.jar" -> s"https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli.jar"),
    )

  lazy val `dist-mac-x86_64` = project.in(file("dist/mac-x86_64")).asDist
    .settings(packageName := (dist / packageName).value + "-x86_64-apple-darwin")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli" -> s"gz+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-x86_64-apple-darwin.gz")
    )

  lazy val `dist-mac-aarch64` = project.in(file("dist/mac-aarch64")).asDist
    .settings(packageName := (dist / packageName).value + "-aarch64-apple-darwin")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli" -> s"gz+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-aarch64-apple-darwin.gz")
    )

  lazy val `dist-win-x86_64` = project.in(file("dist/win-x86_64")).asDist
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

  lazy val `dist-linux-x86_64` = project.in(file("dist/linux-x86_64")).asDist
    .settings(packageName := (dist / packageName).value + "-x86_64-pc-linux")
    .settings(
      republishLibexecDir := (dist / republishLibexecDir).value,
      republishLibexecOverrides += (dist / baseDirectory).value / "libexec-native-overrides",
      republishFetchCoursier := (dist / republishFetchCoursier).value,
      republishLaunchers +=
        ("scala-cli" -> s"gz+https://github.com/VirtusLab/scala-cli/releases/download/v$scalaCliLauncherVersion/scala-cli-x86_64-pc-linux.gz")
    )

  lazy val `dist-linux-aarch64` = project.in(file("dist/linux-aarch64")).asDist
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

    /*def asDottyBench(implicit mode: Mode): Project = project.withCommonSettings.
      dependsOn(dottyCompiler).
      settings(commonBenchmarkSettings).
      enablePlugins(JmhPlugin)*/

    def asDist: Project = project
      .enablePlugins(UniversalPlugin, RepublishPlugin)
      .settings(commonBootstrappedSettings)
      .settings(commonDistSettings)
      .dependsOn(
        `scala-library-bootstrapped`,
        `scala3-compiler-bootstrapped`,
        `scala3-interfaces`,
        `scala3-library-bootstrapped`,
        `scala3-repl`,
        `scala3-sbt-bridge-bootstrapped`, // for scala-cli
        `scala3-staging`,
        `scala3-tasty-inspector`,
        scaladoc,
        `tasty-core-bootstrapped`,
      )
      .settings(
        target := baseDirectory.value / "target" // override setting in commonBootstrappedSettings
      )

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

  /** Helper to validate JAR contents */
  private def validateJarIsEmpty(jar: File): File = {
    val jarFile = new java.util.jar.JarFile(jar)
    try {
      import scala.jdk.CollectionConverters._
      val nonMetaInfEntries = jarFile.entries().asScala
        .map(_.getName)
        .filterNot(name => name.startsWith("META-INF/") || name == "META-INF")
        .toList

      if (nonMetaInfEntries.nonEmpty) {
        val entriesList = nonMetaInfEntries.take(10).mkString("\n  - ", "\n  - ", "")
        val truncated = if (nonMetaInfEntries.size > 10) s"\n  ... and ${nonMetaInfEntries.size - 10} more" else ""
        sys.error(
          s"""JAR ${jar.getName} should only contain META-INF entries but found ${nonMetaInfEntries.size} other entries:$entriesList$truncated
             |
             |This artifact is intended to be an empty placeholder that only declares dependencies.
             |If you need to add content, please verify this is intentional.""".stripMargin
        )
      }
    } finally jarFile.close()
    jar
  }

  /** Settings for projects that should produce empty JARs (only META-INF allowed).
   *  These are dependency placeholder projects like scala3-library-bootstrapped and scala3-library-sjs.
   *  Validates: .jar, -sources.jar, and -javadoc.jar
   */
  lazy val emptyPublishedJarSettings = Def.settings(
    Compile / sources := Seq(),
    Compile / resources := Seq(),
    Test / sources := Seq(),
    Test / resources := Seq(),
    Compile / packageBin := (Compile / packageBin).map(validateJarIsEmpty).value,
    Compile / packageSrc := (Compile / packageSrc).map(validateJarIsEmpty).value,
    Compile / packageDoc := (Compile / packageDoc).map(validateJarIsEmpty).value,
  )
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
    val stdlib = { // relative path to the stdlib directory ('library/')
      val projectRoot = (ThisBuild/baseDirectory).value.toPath
      val stdlibRoot = (`scala-library-bootstrapped` / baseDirectory).value
      projectRoot.relativize(stdlibRoot.toPath.normalize())
    }

    DefaultGenerationSettings.value
      .add(ProjectName("Scala 3"))
      .add(OutputDir(file("scaladoc/output/scala3").getAbsoluteFile.getAbsolutePath))
      .add(Revision("main"))
      .add(ExternalMappings(List(javaExternalMapping)))
      .add(DocRootContent((stdlib / "src" / "rootdoc.txt").toString))
      .add(CommentSyntax(List(
        // Only the files below use markdown syntax (Scala 3 specific sources)
        s"$stdlib/src/scala/NamedTuple.scala=markdown",
        s"$stdlib/src/scala/Tuple.scala=markdown",
        s"$stdlib/src/scala/compiletime=markdown",
        s"$stdlib/src/scala/quoted=markdown",
        s"$stdlib/src/scala/util/boundary.scala=markdown",
        // Scala 2 sources use wiki syntax, we keep it as the default
        "wiki"
      )))
      .add(VersionsDictionaryUrl("https://scala-lang.org/api/versions.json"))
      .add(DocumentSyntheticTypes(true))
      .add(SnippetCompiler(List(
        s"$stdlib/src/scala/compiletime=compile",
        s"$stdlib/src/scala/quoted=compile",
        s"$stdlib/src/scala/util/control=compile",
        s"$stdlib/src/scala/util=compile",
        s"$stdlib/src/scala=compile",
      )))
      .add(SiteRoot("docs"))
      .add(ApiSubdirectory(true))
      .withTargets((`scala-library-bootstrapped` / Compile / products).value.map(_.getAbsolutePath))
  }


}
