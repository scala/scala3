package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8
import org.junit.{Ignore, Test}
import org.junit.Assert.{assertEquals, fail}
import org.junit.experimental.categories.Category


lazy val communitybuildDir: Path = Paths.get(sys.props("user.dir"))

lazy val compilerVersion: String =
  val file = communitybuildDir.resolve("dotty-bootstrapped.version")
  new String(Files.readAllBytes(file), UTF_8)

lazy val sbtPluginFilePath: String =
  // Workaround for https://github.com/sbt/sbt/issues/4395
  new File(sys.props("user.home") + "/.sbt/1.0/plugins").mkdirs()
  communitybuildDir.resolve("sbt-dotty-sbt").toAbsolutePath().toString()

def log(msg: String) = println(Console.GREEN + msg + Console.RESET)

/** Executes shell command, returns false in case of error. */
def exec(projectDir: Path, binary: String, arguments: String*): Int =
  val command = binary +: arguments
  log(command.mkString(" "))
  val builder = new ProcessBuilder(command: _*).directory(projectDir.toFile).inheritIO()
  val process = builder.start()
  val exitCode = process.waitFor()
  exitCode


sealed trait CommunityProject
  private var published = false

  val project: String
  val updateCommand: String
  val testCommand: String
  val publishCommand: String
  val dependencies: List[CommunityProject]
  val binaryName: String
  val runCommandsArgs: List[String] = Nil

  final val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

  /** Is this project running in the test or update mode in the
   *  context of the given suite? @see `run` for more details.
   */
  final def isUpdateMode(given suite: CommunityBuildTest) =
    suite.isInstanceOf[CommunityBuildUpdate]

  /** Depending on the mode of operation, either
   *  runs the test or updates the project. Updating
   *  means that all the dependencies are fetched but
   *  minimal other extra other work is done. Updating
   *  is necessary since we run tests each time on a fresh
   *  Docker container. We run the update on Docker container
   *  creation time to create the cache of the dependencies
   *  and avoid network overhead. See https://github.com/lampepfl/dotty-drone
   *  for more infrastructural details.
   */
  final def run()(given suite: CommunityBuildTest) =
    val runCmd = if isUpdateMode then updateCommand else testCommand
    if !isUpdateMode then dependencies.foreach(_.publish())
    suite.test(project, binaryName, runCommandsArgs :+ runCmd)

  /** Publish this project to the local Maven repository */
  final def publish(): Unit =
    if !published
      log(s"Publishing $project")
      if publishCommand eq null
        throw RuntimeException(s"Publish command is not specified for $project. Project details:\n$this")
      val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ publishCommand): _*)
      if exitCode != 0
        throw RuntimeException(s"Publish command exited with code $exitCode for project $project. Project details:\n$this")
      published = true
end CommunityProject

final case class MillCommunityProject(project: String, baseCommand: String,
  dependencies: List[CommunityProject] = Nil) extends CommunityProject
  override val binaryName: String = "./mill"
  override val updateCommand = s"$baseCommand.compileClasspath"
  override val testCommand = s"$baseCommand.test"
  override val publishCommand = s"$baseCommand.publishLocal"
  override val runCommandsArgs = List("-i", "-D", s"dottyVersion=$compilerVersion")

final case class SbtCommunityProject(project: String, sbtTestCommand: String,
  sbtUpdateCommand: String, extraSbtArgs: List[String] = Nil,
  dependencies: List[CommunityProject] = Nil, publishCommand: String = null) extends CommunityProject
  override val binaryName: String = "sbt"
  private val baseCommand = s";clean ;set updateOptions in Global ~= (_.withLatestSnapshots(false)) ;++$compilerVersion! "
  override val testCommand = s"$baseCommand$sbtTestCommand"
  override val updateCommand = s"$baseCommand$sbtUpdateCommand"

  override val runCommandsArgs: List[String] =
    // Run the sbt command with the compiler version and sbt plugin set in the build
    val sbtProps = Option(System.getProperty("sbt.ivy.home")) match
      case Some(ivyHome) => List(s"-Dsbt.ivy.home=$ivyHome")
      case _ => Nil
    extraSbtArgs ++ sbtProps ++ List(
      "-sbt-version", "1.2.7",
      s"--addPluginSbtFile=$sbtPluginFilePath")

object projects
  lazy val utest = MillCommunityProject(
    project = "utest",
    baseCommand = s"utest.jvm[$compilerVersion]",
  )

  lazy val sourcecode = MillCommunityProject(
    project = "sourcecode",
    baseCommand = s"sourcecode.jvm[$compilerVersion]",
  )

  lazy val oslib = MillCommunityProject(
    project = "os-lib",
    baseCommand = s"os[$compilerVersion]",
    dependencies = List(utest, sourcecode)
  )

  lazy val oslibWatch = MillCommunityProject(
    project = "os-lib",
    baseCommand = s"os.watch[$compilerVersion]",
    dependencies = List(utest, sourcecode)
  )

  lazy val ujson = MillCommunityProject(
    project = "upickle",
    baseCommand = s"ujson.jvm[$compilerVersion]",
    dependencies = List(scalatest, scalacheck, scalatestplusScalacheck)
  )

  lazy val intent = SbtCommunityProject(
    project       = "intent",
    sbtTestCommand   = "test",
    sbtUpdateCommand = "update"
  )

  lazy val algebra = SbtCommunityProject(
    project       = "algebra",
    sbtTestCommand   = "coreJVM/compile",
    sbtUpdateCommand = "coreJVM/update"
  )

  lazy val scalacheck = SbtCommunityProject(
    project       = "scalacheck",
    sbtTestCommand   = "jvm/test",
    sbtUpdateCommand = "jvm/test:update",
    publishCommand = ";set jvm/publishArtifact in (Compile, packageDoc) := false ;jvm/publishLocal"
  )

  lazy val scalatest = SbtCommunityProject(
    project       = "scalatest",
    sbtTestCommand   = ";scalacticDotty/clean;scalacticTestDotty/test;scalatestTestDotty/test",
    sbtUpdateCommand = "scalatest/update",
    publishCommand = ";scalacticDotty/publishLocal; scalatestDotty/publishLocal"
  )

  lazy val scalatestplusScalacheck = SbtCommunityProject(
    project = "scalatestplus-scalacheck",
    sbtTestCommand = "scalatestPlusScalaCheckJVM/compile",  // TODO: compile only because tests are prone to java.lang.OutOfMemoryError: Metaspace
    sbtUpdateCommand = "scalatestPlusScalaCheckJVM/update",
    publishCommand = "scalatestPlusScalaCheckJVM/publishLocal",
    dependencies = List(scalatest, scalacheck)
  )

  lazy val scalaXml = SbtCommunityProject(
    project       = "scala-xml",
    sbtTestCommand   = "xml/test",
    sbtUpdateCommand = "xml/update"
  )

  lazy val scopt = SbtCommunityProject(
    project       = "scopt",
    sbtTestCommand   = "scoptJVM/compile",
    sbtUpdateCommand = "scoptJVM/update"
  )

  lazy val scalap = SbtCommunityProject(
    project       = "scalap",
    sbtTestCommand   = "scalap/compile",
    sbtUpdateCommand = "scalap/update"
  )

  lazy val squants = SbtCommunityProject(
    project       = "squants",
    sbtTestCommand   = "squantsJVM/compile",
    sbtUpdateCommand = "squantsJVM/update"
  )

  lazy val betterfiles = SbtCommunityProject(
    project       = "betterfiles",
    sbtTestCommand   = "dotty-community-build/compile",
    sbtUpdateCommand = "dotty-community-build/update"
  )

  lazy val ScalaPB = SbtCommunityProject(
    project       = "ScalaPB",
    sbtTestCommand   = "dotty-community-build/compile",
    sbtUpdateCommand = "dotty-community-build/update"
  )

  lazy val minitest = SbtCommunityProject(
    project       = "minitest",
    sbtTestCommand   = "dotty-community-build/compile",
    sbtUpdateCommand = "dotty-community-build/update"
  )

  lazy val fastparse = SbtCommunityProject(
    project       = "fastparse",
    sbtTestCommand   = "dotty-community-build/compile;dotty-community-build/test:compile",
    sbtUpdateCommand = "dotty-community-build/update"
  )

  lazy val stdLib213 = SbtCommunityProject(
    project       = "stdLib213",
    sbtTestCommand   = "library/compile",
    sbtUpdateCommand = "library/update",
    extraSbtArgs  = List("-Dscala.build.compileWithDotty=true")
  )

  lazy val shapeless = SbtCommunityProject(
    project       = "shapeless",
    sbtTestCommand   = "test",
    sbtUpdateCommand = "update"
  )

  lazy val xmlInterpolator = SbtCommunityProject(
    project       = "xml-interpolator",
    sbtTestCommand   = "test",
    sbtUpdateCommand = "update"
  )

  lazy val semanticdb = SbtCommunityProject(
    project       = "semanticdb",
    sbtTestCommand   = "test:compile",
    sbtUpdateCommand = "update"
  )

  lazy val effpi = SbtCommunityProject(
    project       = "effpi",
    // We set `useEffpiPlugin := false` because we don't want to run their
    // compiler plugin since it relies on external binaries (from the model
    // checker mcrl2), however we do compile the compiler plugin.

    // We have to drop the plugin and some akka tests for now, the plugin depends on github.com/bmc/scalasti which
    // has not been updated since 2018, so no 2.13 compat. Some akka tests are dropped due to MutableBehaviour being
    // dropped in the 2.13 compatible release

    // sbtTestCommand   = ";set ThisBuild / useEffpiPlugin := false; effpi/test:compile; plugin/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",
    // sbtUpdateCommand = ";set ThisBuild / useEffpiPlugin := false; effpi/test:update; plugin/test:update; benchmarks/test:update; examples/test:update; pluginBenchmarks/test:update"

    sbtTestCommand   = ";set ThisBuild / useEffpiPlugin := false; effpi/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",
    sbtUpdateCommand = ";set ThisBuild / useEffpiPlugin := false; effpi/test:update; benchmarks/test:update; examples/test:update; pluginBenchmarks/test:update"
  )

  // TODO @odersky? It got broken by #5458
  // val pdbp = test(
  //   project       = "pdbp",
  //   sbtTestCommand   = "compile",
  //   sbtUpdateCommand = "update"
  // )
end projects

@Category(Array(classOf[TestCategory]))
class CommunityBuildTest {
  given CommunityBuildTest = this

  /** Build the given project with the published local compiler and sbt plugin.
   *
   *  This test reads the compiler version from community-build/dotty-bootstrapped.version
   *  and expects community-build/sbt-dotty-sbt to set the compiler plugin.
   *
   *  @param project    The project name, should be a git submodule in community-build/
   *  @param command    The binary file of the program used to test the project – usually
   *                    a build tool like SBT or Mill
   *  @param arguments  Arguments to pass to the testing program
   */
  def test(project: String, command: String, arguments: Seq[String]): Unit = {
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

    val exitCode = exec(projectDir, command, arguments: _*)

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

  @Test def intent = projects.intent.run()
  @Test def algebra = projects.algebra.run()
  @Test def scalacheck = projects.scalacheck.run()
  @Test def scalatest = projects.scalatest.run()
  @Test def scalatestplusScalacheck = projects.scalatestplusScalacheck.run()
  @Test def scalaXml = projects.scalaXml.run()
  @Test def scopt = projects.scopt.run()
  @Test def scalap = projects.scalap.run()
  @Test def squants = projects.squants.run()
  @Test def betterfiles = projects.betterfiles.run()
  @Test def ScalaPB = projects.ScalaPB.run()
  @Test def minitest = projects.minitest.run()
  @Test def fastparse = projects.fastparse.run()
  @Test def utest = projects.utest.run()
  @Test def sourcecode = projects.sourcecode.run()
  @Test def oslib = projects.oslib.run()
  @Test def ujson = projects.ujson.run()
  // @Test def oslibWatch = projects.oslibWatch.run()
  @Test def stdLib213 = projects.stdLib213.run()
  @Test def shapeless = projects.shapeless.run()
  @Test def xmlInterpolator = projects.xmlInterpolator.run()
  @Test def semanticdb = projects.semanticdb.run()
  @Test def effpi = projects.effpi.run()
}

class TestCategory
class UpdateCategory

@Category(Array(classOf[UpdateCategory]))
class CommunityBuildUpdate extends CommunityBuildTest
