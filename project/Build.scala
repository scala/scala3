import sbt.Keys._
import sbt._
import complete.DefaultParsers._
import java.io.{ RandomAccessFile, File }
import java.nio.channels.FileLock
import scala.reflect.io.Path

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.Package.ManifestAttributes

object DottyBuild extends Build {

  val scalacVersion = "2.11.5"

  val dottyOrganization = "ch.epfl.lamp"
  val dottyVersion = {
    val baseVersion = "0.1.1"
    val isNightly = sys.env.get("NIGHTLYBUILD") == Some("yes")
    if (isNightly)
      baseVersion + "-" + VersionUtil.commitDate + "-" + VersionUtil.gitHash + "-NIGHTLY"
    else
      baseVersion + "-SNAPSHOT"
  }

  val jenkinsMemLimit = List("-Xmx1500m")

  val JENKINS_BUILD = "dotty.jenkins.build"
  val DRONE_MEM = "dotty.drone.mem"

  val scalaCompiler = "me.d-d" % "scala-compiler" % "2.11.5-20170111-125332-40bdc7b65a"

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

  // Used to compile files similar to ./bin/dotc script
  lazy val dotc =
    inputKey[Unit]("run the compiler using the correct classpath, or the user supplied classpath")

  // Used to run binaries similar to ./bin/dotr script
  lazy val dotr =
    inputKey[Unit]("run compiled binary using the correct classpath, or the user supplied classpath")

  override def settings: Seq[Setting[_]] = {
    super.settings ++ Seq(
      scalaVersion in Global := scalacVersion,
      version in Global := dottyVersion,
      organization in Global := dottyOrganization,
      organizationName in Global := "LAMP/EPFL",
      organizationHomepage in Global := Some(url("http://lamp.epfl.ch")),
      homepage in Global := Some(url("https://github.com/lampepfl/dotty")),

      // scalac options
      scalacOptions in Global ++= Seq(
        "-feature",
        "-deprecation",
        "-encoding", "UTF8",
        "-language:existentials,higherKinds,implicitConversions"
      ),

      javacOptions in Global ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    )
  }

  /** Enforce 2.11.5. Do not let it be upgraded by dependencies. */
  private val overrideScalaVersionSetting =
    ivyScala := ivyScala.value.map(_.copy(overrideScalaVersion = true))

  // set sources to src/, tests to test/ and resources to resources/
  lazy val sourceStructure = Seq(
    scalaSource       in Compile    := baseDirectory.value / "src",
    scalaSource       in Test       := baseDirectory.value / "test",
    javaSource        in Compile    := baseDirectory.value / "src",
    javaSource        in Test       := baseDirectory.value / "test",
    resourceDirectory in Compile    := baseDirectory.value / "resources"
  )

  // Settings used by all dotty-compiled projects
  lazy val commonBootstrappedSettings = Seq(
    scalaOrganization := dottyOrganization,
    scalaVersion := dottyVersion,
    scalaBinaryVersion := "2.11",
    scalaCompilerBridgeSource :=
      (dottyOrganization % "dotty-sbt-bridge" % scalaVersion.value % "component").sources(),

    // sbt gets very unhappy if two projects use the same target
    target := baseDirectory.value / ".." / "out" / name.value
  )

  /** Projects -------------------------------------------------------------- */

  // Needed because the dotty project aggregates dotty-sbt-bridge but dotty-sbt-bridge
  // currently refers to dotty in its scripted task and "aggregate" does not take by-name
  // parameters: https://github.com/sbt/sbt/issues/2200
  lazy val dottySbtBridgeRef = LocalProject("dotty-sbt-bridge")

  // The root project:
  // - aggregates other projects so that "compile", "test", etc are run on all projects at once.
  // - publishes its own empty artifact "dotty" that depends on "dotty-library" and "dotty-compiler",
  //   this is only necessary for compatibility with sbt which currently hardcodes the "dotty" artifact name
  lazy val dotty = project.in(file(".")).
    // FIXME: we do not aggregate `bin` because its tests delete jars, thus breaking other tests
    aggregate(`dotty-interfaces`, `dotty-library`, `dotty-compiler`, dottySbtBridgeRef,
      `scala-library`, `scala-compiler`, `scala-reflect`, `scalap`).
    dependsOn(`dotty-compiler`).
    dependsOn(`dotty-library`).
    settings(
      addCommandAlias("run", "dotty-compiler/run") ++
      addCommandAlias(
        "partest",
        ";publishLocal" + // Non-bootstrapped dotty needs to be published first
        ";dotty-compiler-bootstrapped/lockPartestFile" +
        ";dotty-compiler-bootstrapped/test:test" +
        ";dotty-compiler-bootstrapped/runPartestRunner"
      ) ++
      addCommandAlias(
        "partest-only",
        ";publishLocal" + // Non-bootstrapped dotty needs to be published first
        ";dotty-compiler-bootstrapped/lockPartestFile" +
        ";dotty-compiler-bootstrapped/test:test-only dotc.tests" +
        ";dotty-compiler-bootstrapped/runPartestRunner"
      ) ++
      addCommandAlias(
        "partest-only-no-bootstrap",
        ";dotty-compiler/lockPartestFile" +
        ";dotty-compiler/test:test-only dotc.tests" +
        ";dotty-compiler/runPartestRunner"
      )
    ).
    settings(publishing)

  // Meta project aggregating all bootstrapped projects
  lazy val `dotty-bootstrapped` = project.
    aggregate(`dotty-library-bootstrapped`, `dotty-compiler-bootstrapped`).
    settings(
      publishArtifact := false
    )

  lazy val `dotty-interfaces` = project.in(file("interfaces")).
    settings(sourceStructure).
    settings(
      // Do not append Scala versions to the generated artifacts
      crossPaths := false,
      // Do not depend on the Scala library
      autoScalaLibrary := false,
      //Remove javac invalid options in Compile doc
      javacOptions in (Compile, doc) --= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    ).
    settings(publishing)

  // Settings shared between dotty-compiler and dotty-compiler-bootstrapped
  lazy val dottyCompilerSettings = Seq(
      // set system in/out for repl
      connectInput in run := true,
      outputStrategy := Some(StdoutOutput),

      // Generate compiler.properties, used by sbt
      resourceGenerators in Compile += Def.task {
        val file = (resourceManaged in Compile).value / "compiler.properties"
        val contents = s"version.number=${version.value}"
        IO.write(file, contents)
        Seq(file)
      }.taskValue,

      // include sources in eclipse (downloads source code for all dependencies)
      //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
      com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

      // get libraries onboard
      partestDeps := Seq(scalaCompiler,
                         "org.scala-lang" % "scala-reflect" % scalacVersion,
                         "org.scala-lang" % "scala-library" % scalacVersion % "test"),
      libraryDependencies ++= partestDeps.value,
      libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1",
                                  "org.scala-lang.modules" %% "scala-partest" % "1.0.11" % "test",
                                  dottyOrganization % "dottydoc-client" % "0.1.0",
                                  "com.vladsch.flexmark" % "flexmark" % "0.10.1",
                                  "com.vladsch.flexmark" % "flexmark-ext-tables" % "0.10.1",
                                  "com.novocode" % "junit-interface" % "0.11" % "test",
                                  "com.github.spullara.mustache.java" % "compiler" % "0.9.3",
                                  "com.typesafe.sbt" % "sbt-interface" % sbtVersion.value),
      // enable improved incremental compilation algorithm
      incOptions := incOptions.value.withNameHashing(true),

      // For convenience, change the baseDirectory when running the compiler
      baseDirectory in (Compile, run) := baseDirectory.value / "..",
      // .. but not when running partest
      baseDirectory in (Test, run) := baseDirectory.value,

      repl := Def.inputTaskDyn {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val dottyLib = packageAll.value("dotty-library")
        (runMain in Compile).toTask(
          s" dotty.tools.dotc.repl.Main -classpath $dottyLib " + args.mkString(" ")
        )
      }.evaluated,

      // Override run to be able to run compiled classfiles
      dotr := {
        val args: Seq[String] = spaceDelimited("<arg>").parsed
        val java: String = Process("which" :: "java" :: Nil) !!
        val scalaLib = (dependencyClasspath in Runtime, packageAll)
          .map { (attList, _) =>
            attList
              .map(_.data.getAbsolutePath)
              .find(_.contains("scala-library"))
              .toList.mkString(":")
          }.value

        if (java == "")
          println("Couldn't find java executable on path, please install java to a default location")
        else if (scalaLib == "") {
          println("Couldn't find scala-library on classpath, please run using script in bin dir instead")
        } else {
          val dottyLib = packageAll.value("dotty-library")
          s"""$java -classpath .:$dottyLib:$scalaLib ${args.mkString(" ")}""" !
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
      testOptions in Test += Tests.Cleanup({ () => partestLockFile.delete }),

      lockPartestFile := {
        // When this file is present, running `test` generates the files for
        // partest. Otherwise it just executes the tests directly.
        val lockDir = partestLockFile.getParentFile
        lockDir.mkdirs
        // Cannot have concurrent partests as they write to the same directory.
        if (lockDir.list.size > 0)
          throw new RuntimeException("ERROR: sbt partest: another partest is already running, pid in lock file: " + lockDir.list.toList.mkString(" "))
        partestLockFile.createNewFile
        partestLockFile.deleteOnExit
      },
      runPartestRunner <<= Def.inputTaskDyn {
        // Magic! This is both an input task and a dynamic task. Apparently
        // command line arguments get passed to the last task in an aliased
        // sequence (see partest alias below), so this works.
        val args = Def.spaceDelimited("<arg>").parsed
        val jars = List(
          (packageBin in Compile).value.getAbsolutePath,
          packageAll.value("dotty-library"),
          packageAll.value("dotty-interfaces")
        ) ++ getJarPaths(partestDeps.value, ivyPaths.value.ivyHome)
        val dottyJars  =
          s"""-dottyJars ${jars.length + 2} dotty.jar dotty-lib.jar ${jars.mkString(" ")}"""
        // Provide the jars required on the classpath of run tests
        runTask(Test, "dotty.partest.DPConsoleRunner", dottyJars + " " + args.mkString(" "))
      },

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
      javaOptions <++= (dependencyClasspath in Runtime, packageAll) map { (attList, pA) =>
        // put needed dependencies on classpath:
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
          // FIXME: when we snip the cord, this should go bye-bye
          if path.contains("scala-library") ||
            // FIXME: currently needed for tests referencing scalac internals
            path.contains("scala-reflect") ||
            // FIXME: currently needed for tests referencing scalac internals
            path.contains("scala-compile") ||
            // FIXME: should go away when xml literal parsing is removed
            path.contains("scala-xml") ||
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

        ("-DpartestParentID=" + pid) :: jars ::: tuning ::: agentOptions ::: ci_build ::: path.toList
      }
  )

  lazy val `dotty-compiler` = project.in(file("compiler")).
    dependsOn(`dotty-interfaces`).
    dependsOn(`dotty-library`).
    settings(sourceStructure).
    settings(dottyCompilerSettings).
    settings(
      overrideScalaVersionSetting,

      // necessary evil: dottydoc currently needs to be included in the dotty
      // project, for sbt integration
      // FIXME: note part of dottyCompilerSettings because the doc-tool does not
      // compile with dotty
      unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
      unmanagedSourceDirectories in Compile += baseDirectory.value / ".." / "doc-tool" / "src",
      unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value),
      unmanagedSourceDirectories in Test += baseDirectory.value / ".." / "doc-tool" / "test",

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
    ).
    settings(publishing)

  lazy val `dotty-compiler-bootstrapped` = project.in(file("compiler")).
    dependsOn(`dotty-library-bootstrapped`).
    settings(sourceStructure).
    settings(commonBootstrappedSettings).
    settings(dottyCompilerSettings).
    settings(
      // Used instead of "dependsOn(`dotty-interfaces`)" because the latter breaks sbt somehow
      libraryDependencies += scalaOrganization.value % "dotty-interfaces" % version.value,

      packageAll := {
        (packageAll in `dotty-compiler`).value ++ Seq(
          ("dotty-compiler" -> (packageBin in Compile).value.getAbsolutePath),
          ("dotty-library" -> (packageBin in (`dotty-library-bootstrapped`, Compile)).value.getAbsolutePath)
        )
      }
    )

  /* Contains unit tests for the scripts */
  lazy val `dotty-bin-tests` = project.in(file("bin")).
    settings(sourceStructure).
    settings(
      publishArtifact := false,
      parallelExecution in Test := false,
      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.11" % "test"
    )

  // Settings shared between dotty-library and dotty-library-bootstrapped
  lazy val dottyLibrarySettings = Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalacVersion,
        "org.scala-lang" % "scala-library" % scalacVersion,
        "com.novocode" % "junit-interface" % "0.11" % "test"
      )
  )

  lazy val `dotty-library` = project.in(file("library")).
    settings(sourceStructure).
    settings(dottyLibrarySettings).
    settings(publishing)

  lazy val `dotty-library-bootstrapped` = project.in(file("library")).
    settings(sourceStructure).
    settings(commonBootstrappedSettings).
    settings(dottyLibrarySettings)

  // until sbt/sbt#2402 is fixed (https://github.com/sbt/sbt/issues/2402)
  lazy val cleanSbtBridge = TaskKey[Unit]("cleanSbtBridge", "delete dotty-sbt-bridge cache")

  lazy val `dotty-sbt-bridge` = project.in(file("sbt-bridge")).
    dependsOn(`dotty-compiler`).
    settings(sourceStructure).
    settings(
      overrideScalaVersionSetting,

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
      publishLocal := (publishLocal.dependsOn(cleanSbtBridge)).value,
      description := "sbt compiler bridge for Dotty",
      resolvers += Resolver.typesafeIvyRepo("releases"),
      libraryDependencies ++= Seq(
        "com.typesafe.sbt" % "sbt-interface" % sbtVersion.value,
        "org.scala-sbt" % "api" % sbtVersion.value % "test",
        "org.specs2" %% "specs2" % "2.3.11" % "test"
      ),
      // The sources should be published with crossPaths := false since they
      // need to be compiled by the project using the bridge.
      crossPaths := false,

      // Don't publish any binaries for the bridge because of the above
      publishArtifact in (Compile, packageBin) := false,

      fork in Test := true,
      parallelExecution in Test := false
    ).
    settings(ScriptedPlugin.scriptedSettings: _*).
    settings(
      ScriptedPlugin.sbtTestDirectory := baseDirectory.value / "sbt-test",
      ScriptedPlugin.scriptedLaunchOpts := Seq("-Xmx1024m"),
      ScriptedPlugin.scriptedBufferLog := false,
      ScriptedPlugin.scripted := {
        val x1 = (publishLocal in `dotty-interfaces`).value
        val x2 = (publishLocal in `dotty-compiler`).value
        val x3 = (publishLocal in `dotty-library`).value
        val x4 = (publishLocal in dotty).value // Needed because sbt currently hardcodes the dotty artifact
        ScriptedPlugin.scriptedTask.evaluated
      }
      // TODO: Use this instead of manually copying DottyInjectedPlugin.scala
      // everywhere once https://github.com/sbt/sbt/issues/2601 gets fixed.
      /*,
      ScriptedPlugin.scriptedPrescripted := { f =>
        IO.write(inj, """
import sbt._
import Keys._

object DottyInjectedPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  override val projectSettings = Seq(
    scalaVersion := "0.1.1-SNAPSHOT",
    scalaOrganization := "ch.epfl.lamp",
    scalacOptions += "-language:Scala2",
    scalaBinaryVersion  := "2.11",
    autoScalaLibrary := false,
    libraryDependencies ++= Seq("org.scala-lang" % "scala-library" % "2.11.5"),
    scalaCompilerBridgeSource := ("ch.epfl.lamp" % "dotty-sbt-bridge" % scalaVersion.value % "component").sources()
  )
}
""")
      }
      */
    ).
    settings(publishing)

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
    settings(sourceStructure).
    settings(
      overrideScalaVersionSetting,

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
      jsDependencyManifest <<= jsDependencyManifest.dependsOn(compile)
    )))

  lazy val `dotty-bench` = project.in(file("bench")).
    dependsOn(`dotty-compiler` % "compile->test").
    settings(sourceStructure).
    settings(
      overrideScalaVersionSetting,

      baseDirectory in (Test,run) := (baseDirectory in `dotty-compiler`).value,

      libraryDependencies ++= Seq(
        scalaCompiler % Test,
        "com.storm-enroute" %% "scalameter" % "0.6" % Test
      ),

      fork in Test := true,
      parallelExecution in Test := false,

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      javaOptions <++= (dependencyClasspath in Runtime, packageBin in Compile) map { (attList, bin) =>
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


  // Dummy scala-library artefact. This is useful because sbt projects
  // automatically depend on scalaOrganization.value % "scala-library" % scalaVersion.value
  lazy val `scala-library` = project.
    dependsOn(`dotty-library`).
    settings(
      crossPaths := false
    ).
    settings(publishing)

  // sbt >= 0.13.12 will automatically rewrite transitive dependencies on
  // any version in any organization of scala{-library,-compiler,-reflect,p}
  // to have organization `scalaOrganization` and version `scalaVersion`
  // (see https://github.com/sbt/sbt/pull/2634).
  // This means that we need to provide dummy artefacts for these projects,
  // otherwise users will get compilation errors if they happen to transitively
  // depend on one of these projects.
  lazy val `scala-compiler` = project.
    settings(
      crossPaths := false,
      libraryDependencies := Seq(scalaCompiler)
    ).
    settings(publishing)
  lazy val `scala-reflect` = project.
    settings(
      crossPaths := false,
      libraryDependencies := Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    ).
    settings(publishing)
  lazy val `scalap` = project.
    settings(
      crossPaths := false,
      libraryDependencies := Seq("org.scala-lang" % "scalap" % scalaVersion.value)
    ).
    settings(publishing)

   lazy val publishing = Seq(
     publishMavenStyle := true,
     publishArtifact := true,
     isSnapshot := version.value.contains("SNAPSHOT"),
     publishTo := {
       val nexus = "https://oss.sonatype.org/"
       if (isSnapshot.value)
         Some("snapshots" at nexus + "content/repositories/snapshots")
       else
         Some("releases"  at nexus + "service/local/staging/deploy/maven2")
     },
     publishArtifact in Test := false,
     homepage := Some(url("https://github.com/lampepfl/dotty")),
     licenses += ("BSD New",
       url("https://github.com/lampepfl/dotty/blob/master/LICENSE.md")),
     scmInfo := Some(
       ScmInfo(
         url("https://github.com/lampepfl/dotty"),
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

  // Partest tasks
  lazy val lockPartestFile = TaskKey[Unit]("lockPartestFile", "Creates the lock file at ./tests/locks/partest-<pid>.lock")
  lazy val partestLockFile = new File("." + File.separator + "tests" + File.separator + "locks" + File.separator + s"partest-$pid.lock")
  def pid = java.lang.Long.parseLong(java.lang.management.ManagementFactory.getRuntimeMXBean().getName().split("@")(0))

  lazy val runPartestRunner = InputKey[Unit]("runPartestRunner", "Runs partest")

  lazy val partestDeps = SettingKey[Seq[ModuleID]]("partestDeps", "Finds jars for partest dependencies")
  def getJarPaths(modules: Seq[ModuleID], ivyHome: Option[File]): Seq[String] = ivyHome match {
    case Some(home) =>
      modules.map({ module =>
        val file = Path(home) / Path("cache") /
          Path(module.organization) / Path(module.name) / Path("jars") /
          Path(module.name + "-" + module.revision + ".jar")
        if (!file.isFile) throw new RuntimeException("ERROR: sbt getJarPaths: dependency jar not found: " + file)
        else file.jfile.getAbsolutePath
      })
    case None => throw new RuntimeException("ERROR: sbt getJarPaths: ivyHome not defined")
  }

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
}
