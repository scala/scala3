package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8
import org.junit.{Ignore, Test}
import org.junit.Assert.{assertEquals, fail}
import org.junit.experimental.categories.Category

@Category(Array(classOf[TestCategory]))
class CommunityBuildTest {
  lazy val communitybuildDir: Path = Paths.get(sys.props("user.dir") + "/community-build/")

  lazy val compilerVersion: String = {
    val file = communitybuildDir.resolve("dotty-bootstrapped.version")
    new String(Files.readAllBytes(file), UTF_8)
  }

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
  def test(project: String, testCommand: String, updateCommand: String, extraSbtArgs: Seq[String] = Nil): Unit = {
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

    val exitCode = exec("sbt", arguments: _*)

    if (exitCode != 0) {
      fail(s"""
          |
          |sbt exited with an error code. To reproduce without JUnit, use:
          |
          |    sbt community-build/prepareCommunityBuild
          |    cd community-build/community-projects/$project
          |    sbt ${arguments.init.mkString(" ")} "${arguments.last}"
          |
          |For a faster feedback loop, one can try to extract a direct call to dotc
          |using the sbt export command. For instance, for scalacheck, use
          |    sbt export jvm/test:compileIncremental
          |
          |""".stripMargin)
    }
  }

  @Test def algebra = test(
    project       = "algebra",
    testCommand   = "coreJVM/compile",
    updateCommand = "coreJVM/update"
  )

  @Test def scalacheck = test(
    project       = "scalacheck",
    testCommand   = "jvm/test:compile",
    updateCommand = "jvm/test:update"
  )

  @Test def scalatest = test(
    project       = "scalatest",
    testCommand   = ";scalacticDotty/clean;scalacticTestDotty/test;scalatestTestDotty/test",
    updateCommand = "scalatest/update"
  )

  @Test def scopt = test(
    project       = "scopt",
    testCommand   = "scoptJVM/compile",
    updateCommand = "scoptJVM/update"
  )

  @Test def scalap = test(
    project       = "scalap",
    testCommand   = "scalap/compile",
    updateCommand = "scalap/update"
  )

  @Test def squants = test(
    project       = "squants",
    testCommand   = "squantsJVM/compile",
    updateCommand = "squantsJVM/update"
  )

  @Test def betterfiles = test(
    project       = "betterfiles",
    testCommand   = "dotty-community-build/compile",
    updateCommand = "dotty-community-build/update"
  )

  @Test def ScalaPB = test(
    project       = "ScalaPB",
    testCommand   = "dotty-community-build/compile",
    updateCommand = "dotty-community-build/update"
  )

  @Test def minitest = test(
    project       = "minitest",
    testCommand   = "dotty-community-build/compile",
    updateCommand = "dotty-community-build/update"
  )

  @Test def fastparse = test(
    project       = "fastparse",
    testCommand   = "fastparseJVM/compile",
    updateCommand = "fastparseJVM/update"
  )

  // TODO: revert to sourcecodeJVM/test
  @Test def sourcecode = test(
    project       = "sourcecode",
    testCommand   = "sourcecodeJVM/compile",
    updateCommand = "sourcecodeJVM/update"
  )

  @Test def stdLib213 = test(
    project       = "stdLib213",
    testCommand   = "library/compile",
    updateCommand = "library/update",
    extraSbtArgs  = Seq("-Dscala.build.compileWithDotty=true")
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
  override def test(project: String, testCommand: String, updateCommand: String, extraSbtArgs: Seq[String]): Unit =
    super.test(project, updateCommand, null, extraSbtArgs)
}
