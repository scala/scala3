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
  communitybuildDir.resolve("sbt-injected-plugins").toAbsolutePath().toString()

def log(msg: String) = println(Console.GREEN + msg + Console.RESET)

/** Executes shell command, returns false in case of error. */
def exec(projectDir: Path, binary: String, arguments: Seq[String], environment: Map[String, String]): Int =
  import scala.jdk.CollectionConverters._
  val command = binary +: arguments
  log(command.mkString(" "))
  val builder = new ProcessBuilder(command*).directory(projectDir.toFile).inheritIO()
  builder.environment.putAll(environment.asJava)
  val process = builder.start()
  val exitCode = process.waitFor()
  exitCode


sealed trait CommunityProject:
  val project: String
  val testCommand: String
  val publishCommand: String
  val docCommand: String
  val binaryName: String
  val runCommandsArgs: List[String] = Nil
  val environment: Map[String, String] = Map.empty

  final val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

  /** Publish this project to the local Maven repository */
  final def publish(): Unit =
    log(s"Publishing $project")
    if publishCommand eq null then
      throw RuntimeException(s"Publish command is not specified for $project. Project details:\n$this")
    val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ publishCommand), environment)
    if exitCode != 0 then
      throw RuntimeException(s"Publish command exited with code $exitCode for project $project. Project details:\n$this")

  final def doc(): Unit =
    log(s"Documenting $project")
    if docCommand eq null then
      throw RuntimeException(s"Doc command is not specified for $project. Project details:\n$this")
    val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ docCommand), environment)
    if exitCode != 0 then
      throw RuntimeException(s"Doc command exited with code $exitCode for project $project. Project details:\n$this")

  final def build(): Int = exec(projectDir, binaryName, buildCommands, environment)

  final def buildCommands = runCommandsArgs :+ testCommand

end CommunityProject

sealed case class MillCommunityProject(
    project: String,
    baseCommand: String,
    ignoreDocs: Boolean = false,
    executeTests: Boolean = true,
  ) extends CommunityProject:
  override val binaryName: String = "./mill"
  override val testCommand = {
     if executeTests then s"$baseCommand.test"
     else s"$baseCommand.test.compile"
  }
  override val publishCommand = s"$baseCommand.publishLocal"
  override val docCommand = null
    // uncomment once mill is released
    // if ignoreDocs then null else s"$baseCommand.docJar"
  override val runCommandsArgs = List("-i", "-D", s"dottyVersion=$compilerVersion")
  override val environment = Map.empty

final case class SbtCommunityProject(
    project: String,
    sbtTestCommand: String,
    extraSbtArgs: List[String] = Nil,
    sbtPublishCommand: String = null,
    sbtDocCommand: String = null,
    scalacOptions: List[String] = SbtCommunityProject.scalacOptions,
    override val environment: Map[String, String] = Map.empty,
  ) extends CommunityProject:
  override val binaryName: String = "sbt"

  private def scalacOptionsString: String =
    scalacOptions.map("\"" + _ + "\"").mkString("List(", ",", ")")

  private val baseCommand =
    "clean; set Global/logLevel := Level.Error; set Global/updateOptions ~= (_.withLatestSnapshots(false)); "
    ++ (if scalacOptions.isEmpty then "" else s"""set Global/scalacOptions ++= $scalacOptionsString;""")
    ++ s"++$compilerVersion!; "

  override val testCommand =
    """set Global/testOptions += Tests.Argument(TestFramework("munit.Framework"), "+l"); """
    ++ s"$baseCommand$sbtTestCommand"

  override val publishCommand =
    if sbtPublishCommand eq null then null else s"$baseCommand$sbtPublishCommand"

  override val docCommand =
    if sbtDocCommand eq null then null else
      val cmd = if sbtDocCommand.startsWith(";") then sbtDocCommand else s";$sbtDocCommand"
      s"$baseCommand set every useScaladoc := true; set every doc/logLevel := Level.Warn $cmd "

  override val runCommandsArgs: List[String] =
    // Run the sbt command with the compiler version and sbt plugin set in the build
    val sbtProps = Option(System.getProperty("sbt.ivy.home")) match
      case Some(ivyHome) => List(s"-Dsbt.ivy.home=$ivyHome")
      case _ => Nil
    extraSbtArgs ++ sbtProps ++ List("-sbt-version", "1.11.5", "-Dsbt.supershell=false", s"--addPluginSbtFile=$sbtPluginFilePath")

object SbtCommunityProject:
  def scalacOptions = List(
    "-Xcheck-macros",
    "-Wsafe-init",
  )

object projects:

  private def forceDoc(projects: String*) =
    projects.map(project =>
      s""";set $project/Compile/doc/sources ++= ($project/Compile/doc/dotty.tools.sbtplugin.DottyPlugin.autoImport.tastyFiles).value ;$project/doc"""
    ).mkString(" ")

  private def removeRelease8(projects: String*): String =
    projects.map(project =>
      s"""set $project/Compile/scalacOptions := ($project/Compile/scalacOptions).value.filterNot(opt => opt == "-release" || opt == "8")"""
    ).mkString("; ")

  private def aggregateDoc(in: String)(projects: String*) =
    val tastyFiles =
      (in +: projects).map(p => s"($p/Compile/doc/dotty.tools.sbtplugin.DottyPlugin.autoImport.tastyFiles).value").mkString(" ++ ")
    s""";set $in/Compile/doc/sources ++= file("a.scala") +: ($tastyFiles) ;$in/doc"""

  lazy val utest = MillCommunityProject(
    project = "utest",
    baseCommand = s"utest.jvm[$compilerVersion]",
    ignoreDocs = true
  )

  lazy val sourcecode = new MillCommunityProject(
    project = "sourcecode",
    baseCommand = s"sourcecode.jvm[$compilerVersion]",
    ignoreDocs = true,
  ) {
    override val testCommand = s"$baseCommand.test.run"
  }

  lazy val oslib = MillCommunityProject(
    project = "os-lib",
    baseCommand = s"os.jvm[$compilerVersion]",
  )

  lazy val oslibWatch = MillCommunityProject(
    project = "os-lib",
    baseCommand = s"os.watch[$compilerVersion]",
    ignoreDocs = true
  )

  lazy val ujson = MillCommunityProject(
    project = "upickle",
    baseCommand = s"ujson.jvm[$compilerVersion]",
  )

  lazy val upickle = MillCommunityProject(
    project = "upickle",
    baseCommand = s"upickle.jvm[$compilerVersion]",
  )

  lazy val upickleCore = MillCommunityProject(
    project = "upickle",
    baseCommand = s"core.jvm[$compilerVersion]",
  )

  lazy val upickleImplicits = MillCommunityProject(
    project = "upickle",
    baseCommand = s"implicits.jvm[$compilerVersion]",
  )

  lazy val upack = MillCommunityProject(
    project = "upickle",
    baseCommand = s"upack.jvm[$compilerVersion]",
  )

  lazy val geny = MillCommunityProject(
    project = "geny",
    baseCommand = s"geny.jvm[$compilerVersion]",
  )

  lazy val fansi = MillCommunityProject(
    project = "fansi",
    baseCommand = s"fansi.jvm[$compilerVersion]",
    ignoreDocs = true
  )

  lazy val pprint = MillCommunityProject(
    project = "PPrint",
    baseCommand = s"pprint.jvm[$compilerVersion]",
    ignoreDocs = true
  )

  lazy val requests = MillCommunityProject(
    project = "requests",
    baseCommand = s"requests.jvm[$compilerVersion]",
    executeTests = false, // TODO: fix this to pass consistently
  )

  lazy val cask = MillCommunityProject(
    project = "cask",
    baseCommand = s"cask[$compilerVersion]",
  )

  lazy val scas = MillCommunityProject(
    project = "scas",
    baseCommand = "scas.application",
  )

  lazy val intent = SbtCommunityProject(
    project       = "intent",
    sbtTestCommand   = "test",
    sbtDocCommand = "doc",
  )

  lazy val scalacheck = SbtCommunityProject(
    project       = "scalacheck",
    sbtTestCommand   = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal"
  )

  lazy val scalatest: SbtCommunityProject = SbtCommunityProject(
    project       = "scalatest",
    sbtTestCommand   =
      List(
        "scalacticDotty/clean; scalacticDottyJS/clean",

        // Some scalatest's tests are flaky (https://github.com/scalatest/scalatest/issues/2049)
        // so we disable them, this list is based on the one used in the Scala 2 community build
        // (https://github.com/scala/community-build/blob/2.13.x/proj/scalatest.conf).
        """set scalatestTestDotty / Test / managedSources ~= (_.filterNot(_.getName == "GeneratorSpec.scala").filterNot(_.getName == "FrameworkSuite.scala").filterNot(_.getName == "WaitersSpec.scala").filterNot(_.getName == "TestSortingReporterSpec.scala").filterNot(_.getName == "JavaFuturesSpec.scala").filterNot(_.getName == "ParallelTestExecutionSpec.scala").filterNot(_.getName == "TimeLimitsSpec.scala").filterNot(_.getName == "DispatchReporterSpec.scala").filterNot(_.getName == "TestThreadsStartingCounterSpec.scala").filterNot(_.getName == "SuiteSortingReporterSpec.scala").filterNot(_.getName == "CommonGeneratorsSpec.scala").filterNot(_.getName == "PropCheckerAssertingSpec.scala").filterNot(_.getName == "ConductorMethodsSuite.scala").filterNot(_.getName == "EventuallySpec.scala"))""",
        """set scalacticTestDotty / Test / managedSources ~= (_.filterNot(_.getName == "NonEmptyArraySpec.scala"))""",
        """set genRegularTests4 / Test / managedSources ~= (_.filterNot(_.getName == "FrameworkSuite.scala").filterNot(_.getName == "GeneratorSpec.scala").filterNot(_.getName == "CommonGeneratorsSpec.scala").filterNot(_.getName == "ParallelTestExecutionSpec.scala").filterNot(_.getName == "DispatchReporterSpec.scala").filterNot(_.getName == "TestThreadsStartingCounterSpec.scala").filterNot(_.getName == "EventuallySpec.scala"))""",
        "scalacticTestDotty/test; scalatestTestDotty/test; scalacticDottyJS/compile; scalatestDottyJS/compile"
      ).mkString("; "),
    sbtPublishCommand = "scalacticDotty/publishLocal; scalatestDotty/publishLocal; scalacticDottyJS/publishLocal; scalatestDottyJS/publishLocal",
    sbtDocCommand = ";scalacticDotty/doc", // fails with missing type ;scalatestDotty/doc"
  )

  lazy val scalatestplusScalacheck = SbtCommunityProject(
    project = "scalatestplus-scalacheck",
    sbtTestCommand = "scalatestPlusScalaCheckJVM/test",
    sbtPublishCommand = "scalatestPlusScalaCheckJVM/publishLocal",
    sbtDocCommand = "scalatestPlusScalaCheckJVM/doc",
  )

  lazy val scalatestplusJunit = SbtCommunityProject(
    project           = "scalatestplus-junit",
    sbtTestCommand    = "scalatestplus-junit/test",
    sbtPublishCommand = "scalatestplus-junit/publishLocal",
  )

  lazy val scalatestplusTestNG = SbtCommunityProject(
    project = "scalatestplus-testng",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
  )

  lazy val scalaXml = SbtCommunityProject(
    project       = "scala-xml",
    sbtTestCommand = "xml/test",
    sbtPublishCommand = "xml/publishLocal",
    sbtDocCommand = "xml/doc"
  )

  lazy val scalap = SbtCommunityProject(
    project       = "scalap",
    sbtTestCommand   = "scalap/compile",
    sbtDocCommand = "scalap/doc"
  )

  lazy val betterfiles = SbtCommunityProject(
    project       = "betterfiles",
    sbtTestCommand   = "dotty-community-build/compile",
    sbtDocCommand   = ";core/doc ;akka/doc ;shapelessScanner/doc"
  )

  lazy val scalaPB = SbtCommunityProject(
    project       = "ScalaPB",
    sbtTestCommand   = "lensesJVM3/compile; runtimeJVM3/compile; grpcRuntimeJVM3/compile; compilerPluginJVM3/compile",
    // aggregateDoc("runtimeJVM")("scalapbc", "grpcRuntime", "compilerPlugin") fails with
    // module class ScalaPbCodeGenerator$ has non-class parent: TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module protocbridge),ProtocCodeGenerator)
    // Also it seems that we do not handle correctly aggreagation projects
    // sbtDocCommand = "dotty-community-build/doc"
    sbtDocCommand = forceDoc("scalapbc", "grpcRuntime","runtimeJVM", "compilerPlugin")
  )

  lazy val minitest = SbtCommunityProject(
    project       = "minitest",
    sbtTestCommand   = "test",
    sbtDocCommand = aggregateDoc("lawsJVM")("minitestJVM"),
  )

  lazy val fastparse = SbtCommunityProject(
    project       = "fastparse",
    sbtTestCommand   = "dotty-community-build/compile;dotty-community-build/test:compile",
    // Problem parsing perftests/bench2/src/perftests/PythonParse.scala:[0..18..694]
    // sbtDocCommand = "dotty-community-build/doc"
  )

  lazy val shapeless3 = SbtCommunityProject(
    project = "shapeless-3",
    sbtTestCommand = "testJVM; testJS",
    sbtDocCommand = forceDoc("typeable", "deriving"),
    scalacOptions = "-source" :: "3.3" :: SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init"), // due to -Werror
  )

  lazy val xmlInterpolator = SbtCommunityProject(
    project       = "xml-interpolator",
    sbtTestCommand   = "test",
    sbtDocCommand = "doc", // Again we've got problem with extensions
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
    sbtDocCommand    = "set ThisBuild / useEffpiPlugin := false; effpi/doc; benchmarks/doc; examples/doc; pluginBenchmarks/doc",
  )

  // TODO @odersky? It got broken by #5458
  // val pdbp = test(
  //   project       = "pdbp",
  //   sbtTestCommand   = "compile",
  // )

  lazy val sconfig = SbtCommunityProject(
    project       = "sconfig",
    sbtTestCommand   = "sconfigJVM/test",
    sbtDocCommand = "sconfigJVM/doc",
  )

  lazy val zio = SbtCommunityProject(
    project = "zio",
    sbtTestCommand = "testJVMDotty",
    sbtDocCommand = forceDoc("coreJVM"),
    scalacOptions = "-source" :: "3.3" :: SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros"),
  )

  lazy val munit = SbtCommunityProject(
    project = "munit",
    sbtTestCommand  = "testsJVM/test;testsJS/test;",
    sbtPublishCommand = "munitJVM/publishLocal; munitJS/publishLocal; munitScalacheckJVM/publishLocal; munitScalacheckJS/publishLocal; junit/publishLocal",
    sbtDocCommand   = "junit/doc; munitJVM/doc",
  )

  lazy val scodecBits = SbtCommunityProject(
    project          = "scodec-bits",
    sbtTestCommand   = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
    sbtDocCommand   = "coreJVM/doc",
  )

  lazy val scodec = SbtCommunityProject(
    project          = "scodec",
    sbtTestCommand   = "unitTests/test",
    // Adds <empty> package
    sbtDocCommand   = "coreJVM/doc",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init"),
  )

  lazy val scalaParserCombinators = SbtCommunityProject(
    project          = "scala-parser-combinators",
    sbtTestCommand   = "set every versionPolicyIntention := Compatibility.None; parserCombinatorsJVM/test",
    sbtDocCommand   = forceDoc("parserCombinatorsJVM"),
  )

  lazy val dottyCpsAsync = SbtCommunityProject(
    project          = "dotty-cps-async",
    sbtTestCommand   = "test",
    // Does not compile (before reaches doc)
    // sbtDocCommand = "cpsJVM/doc",
  )

  lazy val scalaz = SbtCommunityProject(
    project          = "scalaz",
    sbtTestCommand   = "rootJVM/test",

    // sbtDocCommand = forceDoc("coreJVM"), // Fails with:
    // [error] class scalaz.Conts cannot be unpickled because no class file was found
    // [error] class scalaz.ContsT cannot be unpickled because no class file was found
    // [error] class scalaz.IndexedCont cannot be unpickled because no class file was found

    // aggregateDoc("rootJVM")("effectJVM", "iterateeJVM"), // Fails With
    // [error] Caused by: java.lang.AssertionError: assertion failed:
    // trait MonadIO has non-class parent: AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),module scalaz),Monad),List(TypeRef(ThisType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scalaz)),module effect),trait MonadIO)),type F)))

    // forceDoc("iterateeJVM"), // Fails with
    // [error] class scalaz.iteratee.Iteratee cannot be unpickled because no class file was found

    sbtDocCommand = forceDoc("effectJVM"),
  )

  lazy val endpoints4s = SbtCommunityProject(
    project        = "endpoints4s",
    sbtTestCommand = "json-schemaJVM/compile;algebraJVM/compile;openapiJVM/compile;http4s-server/compile;http4s-client/compile;play-server/compile;play-client/compile;akka-http-server/compile;akka-http-client/compile",
    sbtDocCommand = ";json-schemaJVM/doc ;algebraJVM/doc; openapiJVM/doc; http4s-server/doc ;http4s-client/doc ;play-server/doc ;play-client/doc ;akka-http-server/doc ;akka-http-client/doc",
  )

  lazy val catsEffect3 = SbtCommunityProject(
    project        = "cats-effect-3",
    sbtTestCommand = "ciJVM",
    sbtPublishCommand = "publishLocal",
    sbtDocCommand  = ";coreJVM/doc ;lawsJVM/doc ;kernelJVM/doc",
  )

  lazy val scalaParallelCollections = SbtCommunityProject(
    project        = "scala-parallel-collections",
    sbtTestCommand = "test",
    sbtDocCommand  = forceDoc("core"),
  )

  lazy val scalaCollectionCompat = SbtCommunityProject(
    project        = "scala-collection-compat",
    sbtTestCommand = "compat3/test",
    sbtPublishCommand = "compat3/publishLocal",
  )

  lazy val scalaJava8Compat = SbtCommunityProject(
    project        = "scala-java8-compat",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
  )

  lazy val verify = SbtCommunityProject(
    project        = "verify",
    sbtTestCommand = "verifyJVM/test",
    sbtDocCommand = "verifyJVM/doc",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros") // TODO enable -Xcheck-macros
  )

  lazy val discipline = SbtCommunityProject(
    project = "discipline",
    sbtTestCommand = List(
      removeRelease8("core.jvm", "core.js"),
      "coreJVM/test;coreJS/test"
    ).mkString("; "),
    sbtPublishCommand = "set every credentials := Nil;coreJVM/publishLocal;coreJS/publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init"),
  )

  lazy val disciplineMunit = SbtCommunityProject(
    project = "discipline-munit",
    sbtTestCommand = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
  )

  lazy val disciplineSpecs2 = SbtCommunityProject(
    project = "discipline-specs2",
    sbtTestCommand = "test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init")
  )

  lazy val simulacrumScalafixAnnotations = SbtCommunityProject(
    project = "simulacrum-scalafix",
    sbtTestCommand = "annotation/test:compile;annotationJS/test:compile",
    sbtPublishCommand = "annotation/publishLocal;annotationJS/publishLocal",
  )

  lazy val cats = SbtCommunityProject(
    project = "cats",
    sbtTestCommand = "set Global/scalaJSStage := FastOptStage;rootJVM/test;rootJS/test",
    sbtPublishCommand = "rootJVM/publishLocal;rootJS/publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init") // turn off -Wsafe-init due to -Werror
  )

  lazy val catsMtl = SbtCommunityProject(
    project = "cats-mtl",
    sbtTestCommand = "testsJVM/test;testsJS/test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal;lawsJVM/publishLocal;lawsJS/publishLocal",
  )

  lazy val coop = SbtCommunityProject(
    project = "coop",
    sbtTestCommand = "test",
    sbtPublishCommand = "coreJVM/publishLocal;coreJS/publishLocal",
  )

  lazy val scissEqual = SbtCommunityProject(
    project           = "Equal",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissFingerTree = SbtCommunityProject(
    project           = "FingerTree",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissLog = SbtCommunityProject(
    project           = "Log",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissModel = SbtCommunityProject(
    project           = "Model",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissNumbers = SbtCommunityProject(
    project           = "Numbers",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissSerial = SbtCommunityProject(
    project           = "Serial",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissAsyncFile = SbtCommunityProject(
    project           = "AsyncFile",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissSpan = SbtCommunityProject(
    project           = "Span",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scalaSTM = SbtCommunityProject(
    project           = "scala-stm",
    sbtTestCommand    = "rootJVM/test",
    sbtPublishCommand = "rootJVM/publishLocal",
  )

  lazy val scissLucre = SbtCommunityProject(
    project           = "Lucre",
    sbtTestCommand    =
      val subprojects = List("adjunct.jvm", "base.jvm", "confluent.jvm", "core.jvm", "data.jvm", "expr.jvm", "geom.jvm", "bdb", "tests.jvm")
      List(
        subprojects.map(name => s"""set ($name/Compile/compile/scalacOptions) := ($name/Compile/compile/scalacOptions).value.filterNot(opt => opt == "-release" || opt == "8")"""),
        List("adjunctJVM/test;baseJVM/test;confluentJVM/test;coreJVM/test;dataJVM/test;exprJVM/test;geomJVM/test;lucre-bdb/test;testsJVM/test")
      ).flatten.mkString("; "),
    extraSbtArgs      = List("-Dde.sciss.lucre.ShortTests=true"),
    sbtPublishCommand = "adjunctJVM/publishLocal;baseJVM/publishLocal;confluentJVM/publishLocal;coreJVM/publishLocal;dataJVM/publishLocal;exprJVM/publishLocal;geomJVM/publishLocal;lucre-bdb/publishLocal",
  )

  lazy val izumiReflect = SbtCommunityProject(
    project = "izumi-reflect",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
  )

  lazy val perspective = SbtCommunityProject(
    project = "perspective",
    // No library with easy typeclasses to verify data against exist for Dotty, so no tests yet
    // Until then I guess this mainly serves to check that it still compiles at all
    sbtTestCommand = "dottyPerspectiveExamples/compile",
  )

  lazy val akka = SbtCommunityProject(
    project = "akka",
    extraSbtArgs = List(s"-Dakka.build.scalaVersion=$compilerVersion"),
    sbtTestCommand = List(
      "set every targetSystemJdk := true",
      // selectively turn off -Werror due to deprecations
      """set actor/Compile/scalacOptions += "-Werror:false"""",
      """set testkit/Compile/scalacOptions += "-Werror:false"""",
      """set actorTests/Compile/scalacOptions += "-Werror:false"""",
      "akka-actor-tests/Test/compile",
    ).mkString("; "),
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init"),
  )

  lazy val monocle = SbtCommunityProject(
    project = "Monocle",
    sbtTestCommand = "coreJVM/test; macrosJVM/test; testJVM/test",
  )

  lazy val protoquill = SbtCommunityProject(
    project = "protoquill",
    extraSbtArgs  = List("-Dcommunity=true", "-DcommunityRemote=true", "-Dquill.macro.stdout=true"),
    sbtTestCommand = "runCommunityBuild",
    sbtPublishCommand = "publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros") :+ "-language:implicitConversions", // disabled -Xcheck-macros, due to bug in macro
  )

  lazy val onnxScala = SbtCommunityProject(
    project = "onnx-scala",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
  )

  lazy val playJson = SbtCommunityProject(
    project = "play-json",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
  )

  lazy val munitCatsEffect = SbtCommunityProject(
    project = "munit-cats-effect",
    sbtTestCommand = "ce3JVM/test; ce3JS/test",
    sbtPublishCommand = "ce3JVM/publishLocal; ce3JS/publishLocal",
  )

  lazy val scalacheckEffect = SbtCommunityProject(
    project = "scalacheck-effect",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
  )

  lazy val fs2 = SbtCommunityProject(
    project = "fs2",
    sbtTestCommand =
      List(
        removeRelease8("coreJVM", "coreJS"), // io/test currently fails JDK9+
        "coreJVM/test; coreJS/test;"
      ).mkString("; "),
    sbtPublishCommand = "coreJVM/publishLocal; coreJS/publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init"),
    environment = Map("GITHUB_ACTIONS" -> "false"),
  )

  lazy val libretto = SbtCommunityProject(
    project = "libretto",
    sbtTestCommand = "core/test; examples/compile",
    sbtPublishCommand = "core/publishLocal; examples/publishLocal",
  )

  lazy val jacksonModuleScala = SbtCommunityProject(
    project = "jackson-module-scala",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
  )

  lazy val specs2 = SbtCommunityProject(
    project = "specs2",
    sbtTestCommand = "core/testOnly -- exclude ci",
    sbtPublishCommand = "core/publishLocal",
  )

  lazy val spire = SbtCommunityProject(
    project = "spire",
    sbtTestCommand = "test",
    sbtPublishCommand = "publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros"),
  )

  lazy val http4s = SbtCommunityProject(
    project = "http4s",
    sbtTestCommand = """set ThisBuild / tlFatalWarnings := false; rootJVM/test""",
    sbtPublishCommand = "publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Wsafe-init"),
  )

  lazy val parboiled2 = SbtCommunityProject(
    project = "parboiled2",
    sbtTestCommand = "parboiledCoreJVM/test; parboiledJVM/test",
    sbtPublishCommand = "publishLocal",
    scalacOptions = SbtCommunityProject.scalacOptions.filter(_ != "-Xcheck-macros"),
  )

end projects

def allProjects = List(
  projects.utest,
  projects.sourcecode,
  projects.oslib,
  projects.oslibWatch,
  projects.ujson,
  projects.upickle,
  projects.upickleCore,
  projects.upickleImplicits,
  projects.upack,
  projects.geny,
  projects.fansi,
  projects.pprint,
  projects.requests,
  projects.cask,
  projects.scas,
  projects.intent,
  projects.scalacheck,
  projects.scalatest,
  projects.scalatestplusScalacheck,
  projects.scalatestplusJunit,
  projects.scalaXml,
  projects.scalap,
  projects.betterfiles,
  projects.scalaPB,
  projects.minitest,
  projects.fastparse,
  projects.shapeless3,
  projects.xmlInterpolator,
  projects.effpi,
  projects.sconfig,
  projects.zio,
  projects.munit,
  projects.scodecBits,
  projects.scodec,
  projects.scalaParserCombinators,
  projects.dottyCpsAsync,
  projects.scalaz,
  projects.endpoints4s,
  projects.catsEffect3,
  projects.scalaParallelCollections,
  projects.scalaCollectionCompat,
  projects.scalaJava8Compat,
  projects.verify,
  projects.discipline,
  projects.disciplineMunit,
  projects.disciplineSpecs2,
  projects.simulacrumScalafixAnnotations,
  projects.cats,
  projects.catsMtl,
  projects.coop,
  projects.scissEqual,
  projects.scissFingerTree,
  projects.scissLog,
  projects.scissModel,
  projects.scissNumbers,
  projects.scissSerial,
  projects.scissAsyncFile,
  projects.scissSpan,
  projects.scalaSTM,
  projects.scissLucre,
  projects.izumiReflect,
  projects.perspective,
  projects.akka,
  projects.monocle,
  projects.protoquill,
  projects.onnxScala,
  projects.playJson,
  projects.scalatestplusTestNG,
  projects.munitCatsEffect,
  projects.scalacheckEffect,
  projects.fs2,
  projects.libretto,
  projects.jacksonModuleScala,
  projects.specs2,
  projects.coop,
  projects.spire,
  projects.http4s,
  projects.parboiled2,
)

lazy val projectMap = allProjects.groupBy(_.project)
