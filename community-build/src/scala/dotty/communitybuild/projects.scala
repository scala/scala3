package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8

lazy val communitybuildDir: Path = Paths.get(sys.props("user.dir"))

lazy val compilerVersion: String =
  val file = communitybuildDir.resolve("scala3-bootstrapped.version")
  new String(Files.readAllBytes(file), UTF_8)

lazy val sbtPluginFilePath: String =
  // Workaround for https://github.com/sbt/sbt/issues/4395
  new File(sys.props("user.home") + "/.sbt/1.0/plugins").mkdirs()
  communitybuildDir.resolve("sbt-dotty-sbt").toAbsolutePath().toString()

lazy val sbtScalaJSPluginFilePath: String =
  communitybuildDir.resolve("sbt-scalajs-sbt").toAbsolutePath().toString()

def log(msg: String) = println(Console.GREEN + msg + Console.RESET)

/** Executes shell command, returns false in case of error. */
def exec(projectDir: Path, binary: String, arguments: String*): Int =
  val command = binary +: arguments
  log(command.mkString(" "))
  val builder = new ProcessBuilder(command: _*).directory(projectDir.toFile).inheritIO()
  val process = builder.start()
  val exitCode = process.waitFor()
  exitCode


/** Versions of published projects, needs to be updated when a project in the build is updated. */
object Versions:
  val scalacheck = "1.15.2-SNAPSHOT"
  val scalatest = "3.2.3"
  val munit = "0.7.19+7-3ce72dda-SNAPSHOT"
  val scodecBits = "1.1-17-c6dbf21"

sealed trait CommunityProject:
  private var published = false

  val project: String
  val testCommand: String
  val publishCommand: String
  val dependencies: List[CommunityProject]
  val binaryName: String
  val runCommandsArgs: List[String] = Nil

  final val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

  /** Publish this project to the local Maven repository */
  final def publish(): Unit =
    if !published then
      dependencies.foreach(_.publish())
      log(s"Publishing $project")
      if publishCommand eq null then
        throw RuntimeException(s"Publish command is not specified for $project. Project details:\n$this")
      val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ publishCommand): _*)
      if exitCode != 0 then
        throw RuntimeException(s"Publish command exited with code $exitCode for project $project. Project details:\n$this")
      published = true
end CommunityProject

final case class MillCommunityProject(
    project: String,
    baseCommand: String,
    dependencies: List[CommunityProject] = Nil) extends CommunityProject:
  override val binaryName: String = "./mill"
  override val testCommand = s"$baseCommand.test"
  override val publishCommand = s"$baseCommand.publishLocal"
  override val runCommandsArgs = List("-i", "-D", s"dottyVersion=$compilerVersion")

final case class SbtCommunityProject(
    project: String,
    sbtTestCommand: String,
    extraSbtArgs: List[String] = Nil,
    forceUpgradeSbtScalajsPlugin: Boolean = false,
    dependencies: List[CommunityProject] = Nil,
    sbtPublishCommand: String = null) extends CommunityProject:
  override val binaryName: String = "sbt"

  // A project in the community build can depend on an arbitrary version of
  // another project in the build, so we force the use of the version that is
  // actually in the community build.
  val dependencyOverrides = List(
    // dependencyOverrides doesn't seem to understand `%%%`
    s""""org.scalacheck" %% "scalacheck" % "${Versions.scalacheck}"""",
    s""""org.scalacheck" %% "scalacheck_sjs1" % "${Versions.scalacheck}"""",
    s""""org.scalatest" %% "scalatest" % "${Versions.scalatest}"""",
    s""""org.scalatest" %% "scalatest_sjs1" % "${Versions.scalatest}"""",
    s""""org.scalameta" %% "munit" % "${Versions.munit}"""",
    s""""org.scalameta" %% "munit_sjs1" % "${Versions.munit}"""",
    s""""org.scalameta" %% "munit-scalacheck" % "${Versions.munit}"""",
    s""""org.scalameta" %% "munit-scalacheck_sjs1" % "${Versions.munit}"""",
    s""""org.scalameta" %% "junit-interface" % "${Versions.munit}"""",
    s""""org.scodec" %% "scodec-bits" % "${Versions.scodecBits}"""",
    s""""org.scodec" %% "scodec-bits_sjs1" % "${Versions.scodecBits}"""",
  )

  private val baseCommand =
    "clean; set logLevel in Global := Level.Error; set updateOptions in Global ~= (_.withLatestSnapshots(false)); "
    ++ s"""set dependencyOverrides in ThisBuild ++= ${dependencyOverrides.mkString("Seq(", ", ", ")")}; """
    ++ s"++$compilerVersion!; "

  override val testCommand = s"$baseCommand$sbtTestCommand"
  override val publishCommand = s"$baseCommand$sbtPublishCommand"

  override val runCommandsArgs: List[String] =
    // Run the sbt command with the compiler version and sbt plugin set in the build
    val sbtProps = Option(System.getProperty("sbt.ivy.home")) match
      case Some(ivyHome) => List(s"-Dsbt.ivy.home=$ivyHome")
      case _ => Nil
    val scalaJSPluginArgs =
      if (forceUpgradeSbtScalajsPlugin) List(s"--addPluginSbtFile=$sbtScalaJSPluginFilePath")
      else Nil
    extraSbtArgs ++ sbtProps ++ List(
      "-sbt-version", "1.4.4",
       "-Dsbt.supershell=false",
      s"--addPluginSbtFile=$sbtPluginFilePath"
    ) ++ scalaJSPluginArgs

object projects:
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
    baseCommand = s"os.jvm[$compilerVersion]",
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
    dependencies = List(geny)
  )

  lazy val upickle = MillCommunityProject(
    project = "upickle",
    baseCommand = s"upickle.jvm[$compilerVersion]",
    dependencies = List(geny, utest)
  )

  lazy val upickleCore = MillCommunityProject(
    project = "upickle",
    baseCommand = s"core.jvm[$compilerVersion]",
    dependencies = List(geny, utest)
  )

  lazy val geny = MillCommunityProject(
    project = "geny",
    baseCommand = s"geny.jvm[$compilerVersion]",
    dependencies = List(utest)
  )

  lazy val fansi = MillCommunityProject(
    project = "fansi",
    baseCommand = s"fansi.jvm[$compilerVersion]",
    dependencies = List(utest, sourcecode)
  )

  lazy val pprint = MillCommunityProject(
    project = "PPrint",
    baseCommand = s"pprint.jvm[$compilerVersion]",
    dependencies = List(fansi)
  )

  lazy val requests = MillCommunityProject(
    project = "requests-scala",
    baseCommand = s"requests[$compilerVersion]",
    dependencies = List(geny, utest, ujson, upickleCore)
  )

  lazy val scas = MillCommunityProject(
    project = "scas",
    baseCommand = "scas.application"
  )

  lazy val intent = SbtCommunityProject(
    project       = "intent",
    sbtTestCommand   = "test",
  )

  lazy val algebra = SbtCommunityProject(
    project       = "algebra",
    sbtTestCommand   = "coreJVM/compile",
  )

  lazy val scalacheck = SbtCommunityProject(
    project       = "scalacheck",
    sbtTestCommand   = "jvm/test;js/test",
    sbtPublishCommand = "jvm/publishLocal;js/publishLocal"
  )

  lazy val scalatest = SbtCommunityProject(
    project       = "scalatest",
    sbtTestCommand   = "scalacticDotty/clean;scalacticTestDotty/test; scalatestTestDotty/test",
    sbtPublishCommand = "scalacticDotty/publishLocal; scalatestDotty/publishLocal"
  )

  lazy val scalatestplusScalacheck = SbtCommunityProject(
    project = "scalatestplus-scalacheck",
    sbtTestCommand = "scalatestPlusScalaCheckJVM/test",
    sbtPublishCommand = "scalatestPlusScalaCheckJVM/publishLocal",
    dependencies = List(scalatest, scalacheck)
  )

  lazy val scalaXml = SbtCommunityProject(
    project       = "scala-xml",
    sbtTestCommand   = "xml/test",
  )

  lazy val scalap = SbtCommunityProject(
    project       = "scalap",
    sbtTestCommand   = "scalap/compile",
  )

  lazy val betterfiles = SbtCommunityProject(
    project       = "betterfiles",
    sbtTestCommand   = "dotty-community-build/compile",
  )

  lazy val ScalaPB = SbtCommunityProject(
    project       = "ScalaPB",
    sbtTestCommand   = "dotty-community-build/compile",
  )

  lazy val minitest = SbtCommunityProject(
    project       = "minitest",
    sbtTestCommand   = "test",
    dependencies = List(scalacheck)
  )

  lazy val fastparse = SbtCommunityProject(
    project       = "fastparse",
    sbtTestCommand   = "dotty-community-build/compile;dotty-community-build/test:compile",
  )

  lazy val stdLib213 = SbtCommunityProject(
    project       = "stdLib213",
    extraSbtArgs  = List("-Dscala.build.compileWithDotty=true"),
    sbtTestCommand   = """library/compile""",
    sbtPublishCommand = """set publishArtifact in (library, Compile, packageDoc) := false ;library/publishLocal""",
  )

  lazy val shapeless = SbtCommunityProject(
    project       = "shapeless",
    sbtTestCommand   = "test",
  )

  lazy val xmlInterpolator = SbtCommunityProject(
    project       = "xml-interpolator",
    sbtTestCommand   = "test",
  )

  lazy val effpi = SbtCommunityProject(
    project       = "effpi",
    // We set `useEffpiPlugin := false` because we don't want to run their
    // compiler plugin since it relies on external binaries (from the model
    // checker mcrl2), however we do compile the compiler plugin.

    // We have to drop the plugin and some akka tests for now, the plugin depends on github.com/bmc/scalasti which
    // has not been updated since 2018, so no 2.13 compat. Some akka tests are dropped due to MutableBehaviour being
    // dropped in the 2.13 compatible release

    // sbtTestCommand   = "set ThisBuild / useEffpiPlugin := false; effpi/test:compile; plugin/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",

    sbtTestCommand   = "set ThisBuild / useEffpiPlugin := false; effpi/test:compile; benchmarks/test:compile; examples/test:compile; pluginBenchmarks/test:compile",
  )

  // TODO @odersky? It got broken by #5458
  // val pdbp = test(
  //   project       = "pdbp",
  //   sbtTestCommand   = "compile",
  // )

  lazy val sconfig = SbtCommunityProject(
    project       = "sconfig",
    sbtTestCommand   = "sconfigJVM/test",
  )

  lazy val zio = SbtCommunityProject(
    project = "zio",
    sbtTestCommand = "testJVMDotty",
  )

  lazy val munit = SbtCommunityProject(
    project = "munit",
    sbtTestCommand  = "testsJVM/test;testsJS/test;",
    sbtPublishCommand   = "munitJVM/publishLocal;munitJS/publishLocal;munitScalacheckJVM/publishLocal;munitScalacheckJS/publishLocal;junit/publishLocal",
    dependencies = List(scalacheck)
  )

  lazy val scodecBits = SbtCommunityProject(
    project          = "scodec-bits",
    sbtTestCommand   = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
    dependencies = List(munit)
  )

  lazy val scodec = SbtCommunityProject(
    project          = "scodec",
    sbtTestCommand   = "unitTests/test",
    dependencies = List(munit, scodecBits)
  )

  lazy val scalaParserCombinators = SbtCommunityProject(
    project          = "scala-parser-combinators",
    sbtTestCommand   = "parserCombinatorsJVM/test",
  )

  lazy val dottyCpsAsync = SbtCommunityProject(
    project          = "dotty-cps-async",
    sbtTestCommand   = "test",
  )

  lazy val scalaz = SbtCommunityProject(
    project          = "scalaz",
    sbtTestCommand   = "rootJVM/test",
    dependencies     = List(scalacheck)
  )

  lazy val endpoints4s = SbtCommunityProject(
    project        = "endpoints4s",
    sbtTestCommand = "json-schemaJVM/compile;algebraJVM/compile;openapiJVM/compile;http4s-server/compile;http4s-client/compile;play-server/compile;play-client/compile;akka-http-server/compile;akka-http-client/compile"
  )

  lazy val catsEffect2 = SbtCommunityProject(
    project        = "cats-effect-2",
    sbtTestCommand = "test",
    forceUpgradeSbtScalajsPlugin = true
  )

  lazy val catsEffect3 = SbtCommunityProject(
    project        = "cats-effect-3",
    sbtTestCommand = "testIfRelevant"
  )

  lazy val scalaParallelCollections = SbtCommunityProject(
    project        = "scala-parallel-collections",
    sbtTestCommand = "test",
    dependencies     = List(scalacheck)
  )

  lazy val scalaCollectionCompat = SbtCommunityProject(
    project        = "scala-collection-compat",
    sbtTestCommand = "compat30/test",
  )

end projects
