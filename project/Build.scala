import sbt.Keys._
import sbt._
import java.io.{ RandomAccessFile, File }
import java.nio.channels.FileLock
import scala.reflect.io.Path

object DottyBuild extends Build {

  // Currently, this cannot be increased without hitting the maximum amount of memory
  // available on the Jenkins VMs
  val travisMemLimit = List("-Xmx1100m")

  val JENKINS_BUILD = "dotty.jenkins.build"

  val agentOptions = List(
    // "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
    // "-agentpath:/home/dark/opt/yjp-2013-build-13072/bin/linux-x86-64/libyjpagent.so"
    // "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15052.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
    // "-XX:+HeapDumpOnOutOfMemoryError", "-Xmx1g", "-Xss2m"
  )

  override def settings: Seq[Setting[_]] = {
    super.settings ++ Seq(
      scalaVersion in Global := "2.11.5",
      version in Global := "0.1-SNAPSHOT",
      organization in Global := "org.scala-lang",
      organizationName in Global := "LAMP/EPFL",
      organizationHomepage in Global := Some(url("http://lamp.epfl.ch")),
      homepage in Global := Some(url("https://github.com/lampepfl/dotty")),

      // scalac options
      scalacOptions in Global ++= Seq("-feature", "-deprecation", "-language:existentials,higherKinds,implicitConversions"),

      javacOptions in Global ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")
    )
  }

  lazy val `dotty-interfaces` = project.in(file("interfaces")).
    settings(
      // Do not append Scala versions to the generated artifacts
      crossPaths := false,
      // Do not depend on the Scala library
      autoScalaLibrary := false
    )

  lazy val dotty = project.in(file(".")).
    dependsOn(`dotty-interfaces`).
    settings(
      // set sources to src/, tests to test/ and resources to resources/
      scalaSource in Compile := baseDirectory.value / "src",
      javaSource in Compile := baseDirectory.value / "src",
      scalaSource in Test := baseDirectory.value / "test",
      javaSource in Test := baseDirectory.value / "test",
      resourceDirectory in Compile := baseDirectory.value / "resources",
      unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
      unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value),

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
      partestDeps := Seq("me.d-d" % "scala-compiler" % "2.11.5-20151022-113908-7fb0e653fd",
                         "org.scala-lang" % "scala-reflect" % scalaVersion.value,
                         "org.scala-lang" % "scala-library" % scalaVersion.value % "test"),
      libraryDependencies ++= partestDeps.value,
      libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1",
                                  "org.scala-lang.modules" %% "scala-partest" % "1.0.11" % "test",
                                  "com.novocode" % "junit-interface" % "0.11" % "test",
                                  "jline" % "jline" % "2.12"),

      // enable improved incremental compilation algorithm
      incOptions := incOptions.value.withNameHashing(true),

      // enable verbose exception messages for JUnit
      testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "--run-listener=test.ContextEscapeDetector"),
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
        val jars = Seq((packageBin in Compile).value.getAbsolutePath) ++
            getJarPaths(partestDeps.value, ivyPaths.value.ivyHome)
        val dottyJars  = "-dottyJars " + (jars.length + 1) + " dotty.jar" + " " + jars.mkString(" ")
        // Provide the jars required on the classpath of run tests
        runTask(Test, "dotty.partest.DPConsoleRunner", dottyJars + " " + args.mkString(" "))
      },

      // Adjust classpath for running dotty
      mainClass in (Compile, run) := Some("dotty.tools.dotc.Main"),
      fork in run := true,
      fork in Test := true,
      parallelExecution in Test := false,

      // http://grokbase.com/t/gg/simple-build-tool/135ke5y90p/sbt-setting-jvm-boot-paramaters-for-scala
      javaOptions <++= (dependencyClasspath in Runtime, packageBin in Compile) map { (attList, bin) =>
        // put the Scala {library, reflect} in the classpath
        val path = for {
          file <- attList.map(_.data)
          path = file.getAbsolutePath
        } yield "-Xbootclasspath/p:" + path
        // dotty itself needs to be in the bootclasspath
        val fullpath = ("-Xbootclasspath/a:" + bin) :: path.toList
        // System.err.println("BOOTPATH: " + fullpath)

        val travis_build = // propagate if this is a travis build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}") ::: travisMemLimit
          else
            List()

        val tuning =
          if (sys.props.isDefinedAt("Oshort"))
            // Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
            List("-XX:+TieredCompilation", "-XX:TieredStopAtLevel=1")
          else
            List()

        ("-DpartestParentID=" + pid) :: tuning ::: agentOptions ::: travis_build ::: fullpath
      }
    ).
    settings(
      addCommandAlias("partest",                   ";test:package;package;test:runMain dotc.build;lockPartestFile;test:test;runPartestRunner") ++
      addCommandAlias("partest-only",              ";test:package;package;test:runMain dotc.build;lockPartestFile;test:test-only dotc.tests;runPartestRunner") ++
      addCommandAlias("partest-only-no-bootstrap", ";test:package;package;                        lockPartestFile;test:test-only dotc.tests;runPartestRunner")
    )

  lazy val `dotty-bench` = project.in(file("bench")).
    dependsOn(dotty % "compile->test").
    settings(
      baseDirectory in (Test,run) := (baseDirectory in dotty).value,

      libraryDependencies ++= Seq("com.storm-enroute" %% "scalameter" % "0.6" % Test,
        "com.novocode" % "junit-interface" % "0.11"),
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),

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

        val travis_build = // propagate if this is a travis build
          if (sys.props.isDefinedAt(JENKINS_BUILD))
            List(s"-D$JENKINS_BUILD=${sys.props(JENKINS_BUILD)}")
          else
            List()
        val res = agentOptions ::: travis_build ::: fullpath
        println("Running with javaOptions: " + res)
        res
      }
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
}
