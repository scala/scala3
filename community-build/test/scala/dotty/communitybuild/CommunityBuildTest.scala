package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8
import org.junit.{Ignore, Test}
import org.junit.Assert.{assertEquals, fail}
import org.junit.experimental.categories.Category

@Category(Array(classOf[TestCategory]))
class CommunityBuildTest {
  lazy val communitybuildDir: Path = Paths.get(sys.props("user.dir"))

  lazy val compilerVersion: String = {
    val file = communitybuildDir.resolve("dotty-bootstrapped.version")
    new String(Files.readAllBytes(file), UTF_8)
  }

  def testSbt(project: String, testCommand: String, updateCommand: String, extraSbtArgs: Seq[String] = Nil) = {
    // Workaround for https://github.com/sbt/sbt/issues/4395
    new File(sys.props("user.home") + "/.sbt/1.0/plugins").mkdirs()
    val pluginFilePath = communitybuildDir.resolve("sbt-dotty-sbt").toAbsolutePath().toString()

    // Run the sbt command with the compiler version and sbt plugin set in the build
    val arguments = {
      val sbtProps = Option(System.getProperty("sbt.ivy.home")) match {
        case Some(ivyHome) =>
          Seq(s"-Dsbt.ivy.home=$ivyHome")
        case _ =>
          Seq()
      }
      extraSbtArgs ++ sbtProps ++ Seq(
        "-sbt-version", "1.2.7",
        s"--addPluginSbtFile=$pluginFilePath",
        s";clean ;set updateOptions in Global ~= (_.withLatestSnapshots(false)) ;++$compilerVersion! $testCommand"
      )
    }

    test(project, "sbt", arguments)
  }

  def testMill(project: String, testCommand: String, extraMillArgs: Seq[String] = Nil) =
    test(project, "./mill", extraMillArgs :+ testCommand)

  /** Build the given project with the published local compiler and sbt plugin.
   *
   *  This test reads the compiler version from community-build/dotty-bootstrapped.version
   *  and expects community-build/sbt-dotty-sbt to set the compiler plugin.
   *
   *  @param project        The project name, should be a git submodule in community-build/
   *  @param command        The sbt command used to test the project
   *  @param updateCommand  The sbt command used to update the project
   *  @param extraSbtArgs   Extra arguments to pass to sbt
   */
  def test(project: String, command: String, arguments: Seq[String]): Unit = {
    def log(msg: String) = println(Console.GREEN + msg + Console.RESET)

    log(s"Building $project with dotty-bootstrapped $compilerVersion...")

    val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

    if (!Files.exists(projectDir.resolve(".git"))) {
      fail(s"""
        |
        |Missing $project submodule. You can initialize this module using
        |
        |    git submodule update --init community-build/community-projects/$project
        |
        |""".stripMargin)
    }

    /** Executes shell command, returns false in case of error. */
    def exec(binary: String, arguments: String*): Int = {
      val command = binary +: arguments
      log(command.mkString(" "))
      val builder = new ProcessBuilder(command: _*).directory(projectDir.toFile).inheritIO()
      val process = builder.start()
      val exitCode = process.waitFor()
      exitCode
    }

    val exitCode = exec(command, arguments: _*)

    if (exitCode != 0) {
      fail(s"""
          |
          |$command exited with an error code. To reproduce without JUnit, use:
          |
          |    sbt community-build/prepareCommunityBuild
          |    cd community-build/community-projects/$project
          |    $command ${arguments.init.mkString(" ")} "${arguments.last}"
          |
          |For a faster feedback loop on SBT projects, one can try to extract a direct call to dotc
          |using the sbt export command. For instance, for scalacheck, use
          |    sbt export jvm/test:compileIncremental
          |
          |""".stripMargin)
    }
  }

  @Test def utest = testMill(
    project = "utest",
    testCommand = s"utest.jvm[$compilerVersion].test",
    extraMillArgs = List("-i", "-D", s"dottyVersion=$compilerVersion")
  )

  @Test def intent = testSbt(
    project       = "intent",
    testCommand   = "test",
    updateCommand = "update"
  )

  @Test def algebra = testSbt(
    project       = "algebra",
    testCommand   = "coreJVM/compile",
    updateCommand = "coreJVM/update"
  )

  @Test def scalacheck = testSbt(
    project       = "scalacheck",
    testCommand   = "jvm/test:compile",
    updateCommand = "jvm/test:update"
  )

  @Test def scalatest = testSbt(
    project       = "scalatest",
    testCommand   = ";scalacticDotty/clean;scalacticTestDotty/test;scalatestTestDotty/test",
    updateCommand = "scalatest/update"
  )

  @Test def scalaXml = testSbt(
    project       = "scala-xml",
    testCommand   = "xml/test",
    updateCommand = "xml/update"
  )

  @Test def scopt = testSbt(
    project       = "scopt",
    testCommand   = "scoptJVM/compile",
    updateCommand = "scoptJVM/update"
  )

  @Test def scalap = testSbt(
    project       = "scalap",
    testCommand   = "scalap/compile",
    updateCommand = "scalap/update"
  )

  @Test def squants = testSbt(
    project       = "squants",
    testCommand   = "squantsJVM/compile",
    updateCommand = "squantsJVM/update"
  )

  @Test def betterfiles = testSbt(
    project       = "betterfiles",
    testCommand   = "dotty-community-build/compile",
    updateCommand = "dotty-community-build/update"
  )

  @Test def ScalaPB = testSbt(
    project       = "ScalaPB",
    testCommand   = "dotty-community-build/compile",
    updateCommand = "dotty-community-build/update"
  )

  @Test def minitest = testSbt(
    project       = "minitest",
    testCommand   = "dotty-community-build/compile",
    updateCommand = "dotty-community-build/update"
  )

  @Test def fastparse = testSbt(
    project       = "fastparse",
    testCommand   = "dotty-community-build/compile;dotty-community-build/test:compile",
    updateCommand = "dotty-community-build/update"
  )

  @Test def sourcecode = testMill(
    project = "sourcecode",
    testCommand = s"sourcecode.jvm[$compilerVersion].test",
    extraMillArgs = List("-i", "-D", s"dottyVersion=$compilerVersion")
  )

  @Test def stdLib213 = testSbt(
    project       = "stdLib213",
    testCommand   = "library/compile",
    updateCommand = "library/update",
    extraSbtArgs  = Seq("-Dscala.build.compileWithDotty=true")
  )

  @Test def shapeless = testSbt(
    project       = "shapeless",
    testCommand   = "test",
    updateCommand = "update"
  )

  @Test def xmlInterpolator = testSbt(
    project       = "xml-interpolator",
    testCommand   = "test",
    updateCommand = "update"
  )

  @Test def semanticdb = testSbt(
    project       = "semanticdb",
    testCommand   = "test:compile",
    updateCommand = "update"
  )

  @Test def effpi = testSbt(
    project       = "effpi",
    // We set `useEffpiPlugin := false` because we don't want to run their
    // compiler plugin since it relies on external binaries (from the model
    // checker mcrl2), however we do compile the compiler plugin.

    // We have to drop the plugin and some akka tests for now, the plugin depends on github.com/bmc/scalasti which
    // has not been updated since 2018, so no 2.13 compat. Some akka tests are dropped due to MutableBehaviour being
    // dropped in the 2.13 compatible release

    // testCommand   = ";set ThisBuild / useEffpiPlugin := false; effpi/test:compile; plugin/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",
    // updateCommand = ";set ThisBuild / useEffpiPlugin := false; effpi/test:update; plugin/test:update; benchmarks/test:update; examples/test:update; pluginBenchmarks/test:update"

    testCommand   = ";set ThisBuild / useEffpiPlugin := false; effpi/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",
    updateCommand = ";set ThisBuild / useEffpiPlugin := false; effpi/test:update; benchmarks/test:update; examples/test:update; pluginBenchmarks/test:update"
  )

  // TODO @oderky? It got broken by #5458
  // @Test def pdbp = test(
  //   project       = "pdbp",
  //   testCommand   = "compile",
  //   updateCommand = "update"
  // )
}

class TestCategory
class UpdateCategory

@Category(Array(classOf[UpdateCategory]))
class CommunityBuildUpdate extends CommunityBuildTest {
  override def testSbt(project: String, testCommand: String, updateCommand: String, extraSbtArgs: Seq[String]): Unit =
    super.testSbt(project, updateCommand, null, extraSbtArgs)
}
