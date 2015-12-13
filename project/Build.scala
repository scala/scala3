import sbt.Keys._
import sbt._
import java.io.{ RandomAccessFile, File }
import java.nio.channels.FileLock
import scala.reflect.io.Path

trait BuildUtils {
  def buildOpts(envVarName: String, opts: List[String]) =
    if (sys.props.isDefinedAt(envVarName))  opts
    else                                    Nil
}

trait CIBuild extends BuildUtils {

  val JENKINS_BUILD = "dotty.jenkins.build"

  val travisMemLimit = List("-Xmx1g", "-Xss2m")

  val travisBuild = // propagate if this is a travis build
    buildOpts(JENKINS_BUILD,
      List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}") :::
      travisMemLimit)

  val tuning =
    buildOpts("Oshort",
      // Optimize for short-running applications,
      // see https://github.com/lampepfl/dotty/issues/222
      List("-XX:+TieredCompilation", "-XX:TieredStopAtLevel=1"))

}

trait DebugBuild extends BuildUtils {

    // YOURKIT_AGENT should point to 'libyjpagent.so' location
  val YOURKIT_AGENT = "YOURKIT_AGENT_LIB"

  val agentOptions =
    buildOpts(YOURKIT_AGENT,
      List(
       "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005",
       s"-agentpath:${YOURKIT_AGENT}",
       "-XX:+HeapDumpOnOutOfMemoryError", "-Xmx1g", "-Xss2m"))

}

object DottyBuild extends Build with CIBuild with DebugBuild {

  lazy val dotty = Project(id = "dotty", base = file("."),
    settings = defaults)

  // Note: This version number is being picked up by the stand-alone
  //       dottyc script.
  def scalaCompilerVersion = "2.11.5-20151022-113908-7fb0e653fd"

  lazy val defaults = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion in Global := "2.11.5",
    version in Global      := "0.1-SNAPSHOT",
    organization in Global := "org.scala-lang",
    organizationName in Global     := "LAMP/EPFL",
    organizationHomepage in Global := Some(url("http://lamp.epfl.ch")),
    homepage in Global     := Some(url("https://github.com/lampepfl/dotty")),

    // to get Scala 2.11
    resolvers += Resolver.sonatypeRepo("releases"),

    // set sources to src/, tests to test/ and resources to resources/
    scalaSource in Compile := baseDirectory.value / "src",
    javaSource in Compile  := baseDirectory.value / "src",
    scalaSource in Test    := baseDirectory.value / "test",
    javaSource in Test     := baseDirectory.value / "test",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    unmanagedSourceDirectories in Compile :=
      Seq((scalaSource in Compile).value),
    unmanagedSourceDirectories in Test    := Seq((scalaSource in Test).value),

    // include sources in eclipse (downloads source code for all dependencies)
    // http://stackoverflow.com/questions/10472840/\
    // how-to-attach-sources-to-sbt-managed-dependencies\
    // -in-scala-ide#answer-11683728
    com.typesafe.sbteclipse.plugin.
      EclipsePlugin.EclipseKeys.withSource := true,


    // get libraries onboard
    partestDeps :=
      Seq("me.d-d" % "scala-compiler" % scalaCompilerVersion,
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          "org.scala-lang" % "scala-library" % scalaVersion.value % "test"),
    libraryDependencies ++= partestDeps.value,
    libraryDependencies ++=
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1",
          "org.scala-lang.modules" %% "scala-partest" % "1.0.5" % "test",
          "com.novocode" % "junit-interface" % "0.11" % "test",
          "jline" % "jline" % "2.12"),

    // scalac options
    scalacOptions in Global ++= Seq("-feature", "-deprecation", "-language:_"),
    javacOptions            ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    // enable improved incremental compilation algorithm
    incOptions := incOptions.value.withNameHashing(true),

    mainClass in (Compile, run) := Some("dotty.tools.dotc.Main"),

    // --- Various test tasks and options ---

    // enable verbose exception messages for JUnit
    testOptions in Test += Tests.Argument(
                             TestFrameworks.JUnit, "-a", "-v",
                             "--run-listener=test.ContextEscapeDetector"),
    testOptions in Test += Tests.Cleanup({ () => partestLockFile.delete }),

    lockPartestFile := {
      // When this file is present, running `test` generates the files for
      // partest. Otherwise it just executes the tests directly.
      val lockDir = partestLockFile.getParentFile
      lockDir.mkdirs
      // Cannot have concurrent partests as they write to the same directory.
      if (lockDir.list.size > 0)
        throw new RuntimeException(
          s"""|ERROR: sbt partest: another partest is already running,
              |       pid in lock file: ${lockDir.list.mkString(" ")}""")
      partestLockFile.createNewFile
      partestLockFile.deleteOnExit
    },

    runPartestRunner <<= Def.inputTaskDyn {
      // Magic! This is both an input task and a dynamic task. Apparently
      // command line arguments get passed to the last task in an aliased
      // sequence (see partest alias below), so this works.
      val args = Def.spaceDelimited("<arg>").parsed
      val dottyJars =
        Seq("dotty.jar", (packageBin in Compile).value.getAbsolutePath) ++
        getJarPaths(partestDeps.value, ivyPaths.value.ivyHome)

      val dottyJarsConf  =
        Seq(s"-dottyJars ${dottyJars.length}") ++ dottyJars ++ args
      // Provide the jars required on the classpath of run tests
      runTask(Test, "dotty.partest.DPConsoleRunner",
              dottyJarsConf.mkString(" "))
    },

    fork in run := true,
    fork in Test := true,
    parallelExecution in Test := false,

    // Adjust classpath for running dotty
    // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
    javaOptions <++=
      (managedClasspath in Runtime, packageBin in Compile) map {
        (attList, bin) =>
        // put the Scala {library, reflect} in the classpath
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
        } yield "-Xbootclasspath/p:" + path
        // dotty itself needs to be in the bootclasspath
        val fullpath = ("-Xbootclasspath/a:" + bin) :: path.toList
        // System.err.println("BOOTPATH: " + fullpath)

        ("-DpartestParentID=" + pid) :: tuning ::: agentOptions :::
          travisBuild ::: fullpath
      }
  ) ++ partestAlias ++ partestOnlyAlias

  lazy val partestAlias     = addCommandAlias("partest",
    ";test:package;package;test:runMain dotc.build;" +
    "lockPartestFile;test:test;runPartestRunner")
  lazy val partestOnlyAlias = addCommandAlias("partest-only",
    ";test:package;package;test:runMain dotc.build;" +
    "lockPartestFile;test:test-only dotc.tests;runPartestRunner")


  lazy val benchmarkSettings = Defaults.coreDefaultSettings ++ Seq(

    // to get Scala 2.11
    resolvers += Resolver.sonatypeRepo("releases"),

    baseDirectory in (Test,run) := (baseDirectory in dotty).value,


    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.6" % Test,
      "com.novocode" % "junit-interface" % "0.11"),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),

    scalacOptions in Global ++= Seq("-feature", "-deprecation", "-language:_"),
    javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    fork in Test := true,
    parallelExecution in Test := false,

    // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/\
    //   sbt-setting-jvm-boot-paramaters-for-scala
    javaOptions <++=
      (dependencyClasspath in Runtime, packageBin in Compile) map {
        (attList, bin) =>
          // put the Scala {library, reflect, compiler} in the classpath
          val path = for {
            file <- attList.map(_.data)
            path = file.getAbsolutePath
            prefix = if (path.endsWith(".jar")) "p" else "a"
          } yield "-Xbootclasspath/" + prefix + ":" + path
          // dotty itself needs to be in the bootclasspath
          val fullpath = ("-Xbootclasspath/a:" + bin) :: path.toList
          // System.err.println("BOOTPATH: " + fullpath)

          val travis_build = // propagate if this is a travis build
            if (sys.props.isDefinedAt(JENKINS_BUILD))
              List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}")
            else
              List()
          val opts = agentOptions ::: travis_build ::: fullpath
          println(s"Running with javaOptions: $opts")
          opts
      }
  )


  lazy val benchmarks =
    Project(id = "dotty-bench", settings = benchmarkSettings,
            base = file("bench")) dependsOn(dotty % "compile->test")

  // Partest tasks
  lazy val lockPartestFile =
    TaskKey[Unit]("lockPartestFile",
       "Creates the lock file at ./tests/locks/partest-<pid>.lock")
  lazy val partestLockFile =
    new File(
      List(".", "tests", "locks", s"partest-$pid.lock").
        reduce(_ + File.separator + _))

  def pid = java.lang.Long.parseLong(
    java.lang.management.ManagementFactory.getRuntimeMXBean().
      getName().split("@")(0))

  lazy val runPartestRunner =
    InputKey[Unit]("runPartestRunner", "Runs partest")

  lazy val partestDeps =
    SettingKey[Seq[ModuleID]]("partestDeps",
      "Finds jars for partest dependencies")

  def getJarPaths(modules: Seq[ModuleID], ivyHome: Option[File]) =
    ivyHome map { home =>
      modules map { module =>
        val modJar =
            Path(home) / Path("cache") / Path(module.organization) /
            Path(module.name) / Path("jars") /
            Path(module.name + "-" + module.revision + ".jar")
        if (!modJar.isFile)
          throw new RuntimeException(
            s"ERROR: sbt getJarPaths: dependency jar not found: $modJar")
        else modJar.jfile.getAbsolutePath
      }
    } getOrElse {
      throw new RuntimeException(
        s"ERROR: sbt getJarPaths: ivyHome not defined")
    }
}
