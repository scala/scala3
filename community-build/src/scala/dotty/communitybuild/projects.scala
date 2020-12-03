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

def log(msg: String) = println(Console.GREEN + msg + Console.RESET)

/** Executes shell command, returns false in case of error. */
def exec(projectDir: Path, binary: String, arguments: String*): Int =
  val command = binary +: arguments
  log(command.mkString(" "))
  val builder = new ProcessBuilder(command: _*).directory(projectDir.toFile).inheritIO()
  val process = builder.start()
  val exitCode = process.waitFor()
  exitCode


/** Versions of published projects, needs to be updated when a project in the build is updated.
 *
 *  TODO: instead of harcoding these numbers, we could get them from the
 *  projects themselves. This likely requires injecting a custom task in the
 *  projects to output the version number to a file.
 */
object Versions:
  val discipline = "1.1.3-SNAPSHOT"
  val disciplineMunit = "1.0.3+DOTTY-SNAPSHOT"
  val scalacheck = "1.15.2-SNAPSHOT"
  val scalatest = "3.2.3"
  val munit = "0.7.19+DOTTY-SNAPSHOT"
  val scodecBits = "1.1+DOTTY-SNAPSHOT"

sealed trait CommunityProject:
  private var published = false

  val project: String
  val testCommand: String
  val publishCommand: String
  val docCommand: String
  val dependencies: List[CommunityProject]
  val binaryName: String
  val runCommandsArgs: List[String] = Nil

  final val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

  final def publishDependencies(): Unit =
    dependencies.foreach(_.publish())

  /** Publish this project to the local Maven repository */
  final def publish(): Unit =
    if !published then
      publishDependencies()
      log(s"Publishing $project")
      if publishCommand eq null then
        throw RuntimeException(s"Publish command is not specified for $project. Project details:\n$this")
      val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ publishCommand): _*)
      if exitCode != 0 then
        throw RuntimeException(s"Publish command exited with code $exitCode for project $project. Project details:\n$this")
      published = true

  final def doc(): Unit =
    publishDependencies()
    log(s"Documenting $project")
    if docCommand eq null then
      throw RuntimeException(s"Doc command is not specified for $project. Project details:\n$this")
    val exitCode = exec(projectDir, binaryName, (runCommandsArgs :+ docCommand): _*)
    if exitCode != 0 then
      throw RuntimeException(s"Doc command exited with code $exitCode for project $project. Project details:\n$this")

end CommunityProject

final case class MillCommunityProject(
    project: String,
    baseCommand: String,
    dependencies: List[CommunityProject] = Nil) extends CommunityProject:
  override val binaryName: String = "./mill"
  override val testCommand = s"$baseCommand.test"
  override val publishCommand = s"$baseCommand.publishLocal"
  override val docCommand = null
  override val runCommandsArgs = List("-i", "-D", s"dottyVersion=$compilerVersion")

final case class SbtCommunityProject(
    project: String,
    sbtTestCommand: String,
    extraSbtArgs: List[String] = Nil,
    dependencies: List[CommunityProject] = Nil,
    sbtPublishCommand: String = null,
    sbtDocCommand: String = null
  ) extends CommunityProject:
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
    s""""org.typelevel" %% "discipline-core" % "${Versions.discipline}"""",
    s""""org.typelevel" %% "discipline-core_sjs1" % "${Versions.discipline}"""",
    s""""org.typelevel" %% "discipline-munit" % "${Versions.disciplineMunit}"""",
    s""""org.typelevel" %% "discipline-munit_sjs1" % "${Versions.disciplineMunit}"""",
  )

  private val baseCommand =
    "clean; set logLevel in Global := Level.Error; set updateOptions in Global ~= (_.withLatestSnapshots(false)); "
    ++ s"""set dependencyOverrides in ThisBuild ++= ${dependencyOverrides.mkString("Seq(", ", ", ")")}; """
    ++ s"++$compilerVersion!; "

  override val testCommand = s"$baseCommand$sbtTestCommand"
  override val publishCommand = if sbtPublishCommand eq null then null else s"$baseCommand$sbtPublishCommand"
  override val docCommand =
    if sbtDocCommand eq null then null else
      val cmd = if sbtDocCommand.startsWith(";") then sbtDocCommand else s";$sbtDocCommand"
      s"$baseCommand set every useScala3doc := true; set every doc/logLevel := Level.Warn $cmd "

  override val runCommandsArgs: List[String] =
    // Run the sbt command with the compiler version and sbt plugin set in the build
    val sbtProps = Option(System.getProperty("sbt.ivy.home")) match
      case Some(ivyHome) => List(s"-Dsbt.ivy.home=$ivyHome")
      case _ => Nil
    extraSbtArgs ++ sbtProps ++ List(
      "-sbt-version", "1.4.4",
       "-Dsbt.supershell=false",
      s"--addPluginSbtFile=$sbtPluginFilePath"
    )

object projects:

  private def forceDoc(projects: String*) =
    projects.map(project =>
      s""";set $project/Compile/doc/sources ++= ($project/Compile/doc/tastyFiles).value ;$project/doc"""
    ).mkString(" ")

  private def aggregateDoc(in: String)(projects: String*) =
    val tastyFiles =
      (in +: projects).map(p => s"($p/Compile/doc/tastyFiles).value").mkString(" ++ ")
    s""";set $in/Compile/doc/sources ++= file("a.scala") +: ($tastyFiles) ;$in/doc"""

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
    sbtDocCommand = "doc"
  )

  lazy val algebra = SbtCommunityProject(
    project       = "algebra",
    sbtTestCommand   = "coreJVM/compile",
    sbtDocCommand = forceDoc("coreJVM")
  )

  lazy val scalacheck = SbtCommunityProject(
    project       = "scalacheck",
    sbtTestCommand   = "jvm/test;js/test",
    sbtPublishCommand = "jvm/publishLocal;js/publishLocal",
    sbtDocCommand = forceDoc("jvm")
  )

  lazy val scalatest = SbtCommunityProject(
    project       = "scalatest",
    sbtTestCommand   = "scalacticDotty/clean;scalacticTestDotty/test; scalatestTestDotty/test",
    sbtPublishCommand = "scalacticDotty/publishLocal; scalatestDotty/publishLocal",
    sbtDocCommand = ";scalacticDotty/doc" // fails with missing type ;scalatestDotty/doc"
    // cannot take signature of (test: org.scalatest.concurrent.ConductorFixture#OneArgTest):
    // org.scalatest.Outcome
    // Problem parsing scalatest.dotty/target/scala-3.0.0-M2/src_managed/main/org/scalatest/concurrent/ConductorFixture.scala:[602..624..3843], documentation may not be generated.
    // dotty.tools.dotc.core.MissingType:
  )

  lazy val scalatestplusScalacheck = SbtCommunityProject(
    project = "scalatestplus-scalacheck",
    sbtTestCommand = "scalatestPlusScalaCheckJVM/test",
    sbtPublishCommand = "scalatestPlusScalaCheckJVM/publishLocal",
    sbtDocCommand = "scalatestPlusScalaCheckJVM/doc",
    dependencies = List(scalatest, scalacheck)
  )

  lazy val scalaXml = SbtCommunityProject(
    project       = "scala-xml",
    sbtTestCommand   = "xml/test",
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

  lazy val ScalaPB = SbtCommunityProject(
    project       = "ScalaPB",
    sbtTestCommand   = "dotty-community-build/compile",
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
    dependencies = List(scalacheck)
  )

  lazy val fastparse = SbtCommunityProject(
    project       = "fastparse",
    sbtTestCommand   = "dotty-community-build/compile;dotty-community-build/test:compile",
    // Problem parsing perftests/bench2/src/perftests/PythonParse.scala:[0..18..694]
    // sbtDocCommand = "dotty-community-build/doc"
  )

  lazy val stdLib213 = SbtCommunityProject(
    project       = "stdLib213",
    extraSbtArgs  = List("-Dscala.build.compileWithDotty=true"),
    sbtTestCommand   = """library/compile""",
    sbtPublishCommand = """set publishArtifact in (library, Compile, packageDoc) := false ;library/publishLocal""",
    // sbtDocCommand = "library/doc" // Does no compile? No idea :/
  )


  lazy val shapeless = SbtCommunityProject(
    project       = "shapeless",
    sbtTestCommand   = "test",
    sbtDocCommand = forceDoc("typeable", "deriving", "data")
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
    // sbtDocCommand = "sconfigJVM/doc", // Fails with:
    // Problem parsing sconfig/sharedScala3/src/main/scala/org/ekrich/config/ConfigSyntax.scala:[73..92..1340], documentation may not be generated.
    // scala.MatchError: ValDef(JSON,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class ekrich)),module config),class ConfigSyntax)],Apply(Ident($new),List(Literal(Constant(0)), Literal(Constant(JSON))))) (of class dotty.tools.dotc.ast.Trees$ValDef)
  )

  lazy val zio = SbtCommunityProject(
    project = "zio",
    sbtTestCommand = "testJVMDotty",
    // sbtDocCommand  = forceDoc("coreJVM"),
    // Fails on tasty unpickling https://github.com/lampepfl/dotty/issues/10499
  )

  lazy val munit = SbtCommunityProject(
    project = "munit",
    sbtTestCommand  = "testsJVM/test;testsJS/test;",
    // Hardcode the version to avoid having to deal with something set by sbt-dynver
    sbtPublishCommand   = s"""set every version := "${Versions.munit}"; munitJVM/publishLocal; munitJS/publishLocal; munitScalacheckJVM/publishLocal; munitScalacheckJS/publishLocal; junit/publishLocal""",
    sbtDocCommand   = "munitJVM/doc",
    dependencies = List(scalacheck)
  )

  lazy val scodecBits = SbtCommunityProject(
    project          = "scodec-bits",
    sbtTestCommand   = "coreJVM/test;coreJS/test",
    // Hardcode the version to avoid having to deal with something set by sbt-git
    sbtPublishCommand = s"""set every version := "${Versions.scodecBits}"; coreJVM/publishLocal;coreJS/publishLocal""",
    sbtDocCommand   = "coreJVM/doc",
    dependencies = List(munit)
  )

  lazy val scodec = SbtCommunityProject(
    project          = "scodec",
    sbtTestCommand   = "unitTests/test",
    // Adds <empty> package
    sbtDocCommand   = "coreJVM/doc",
    dependencies = List(munit, scodecBits)
  )

  lazy val scalaParserCombinators = SbtCommunityProject(
    project          = "scala-parser-combinators",
    sbtTestCommand   = "parserCombinatorsJVM/test",
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

    // sbtDocCommand = forceDoc("iterateeJVM"), // Fails with
    // [error] class scalaz.iteratee.Iteratee cannot be unpickled because no class file was found

    sbtDocCommand = forceDoc("effectJVM"),
    dependencies     = List(scalacheck)
  )

  lazy val endpoints4s = SbtCommunityProject(
    project        = "endpoints4s",
    sbtTestCommand = "json-schemaJVM/compile;algebraJVM/compile;openapiJVM/compile;http4s-server/compile;http4s-client/compile;play-server/compile;play-client/compile;akka-http-server/compile;akka-http-client/compile",
    sbtDocCommand = ";json-schemaJVM/doc ;algebraJVM/doc; openapiJVM/doc; http4s-server/doc ;http4s-client/doc ;play-server/doc ;play-client/doc ;akka-http-server/doc ;akka-http-client/doc",
  )

  lazy val catsEffect2 = SbtCommunityProject(
    project        = "cats-effect-2",
    sbtTestCommand = "test",
    // Currently is excluded from community build
    // sbtDocCommand = ";coreJVM/doc ;lawsJVM/doc",
  )

  lazy val catsEffect3 = SbtCommunityProject(
    project        = "cats-effect-3",
    sbtTestCommand = "testIfRelevant",
    // The problem is that testIfRelevant does not compile and project does not compile
    // sbtDocCommand = ";coreJVM/doc ;lawsJVM/doc ;kernelJVM/doc",
  )

  lazy val scalaParallelCollections = SbtCommunityProject(
    project        = "scala-parallel-collections",
    sbtTestCommand = "test",
    sbtDocCommand  = forceDoc("core"),
    dependencies   = List(scalacheck)
  )

  lazy val scalaCollectionCompat = SbtCommunityProject(
    project        = "scala-collection-compat",
    sbtTestCommand = "compat30/test",
  )

  lazy val verify = SbtCommunityProject(
    project        = "verify",
    sbtTestCommand = "verifyJVM/test",
    sbtDocCommand = "verifyJVM/doc",
  )

  lazy val discipline = SbtCommunityProject(
    project = "discipline",
    sbtTestCommand = "coreJVM/test;coreJS/test",
    sbtPublishCommand = "set every credentials := Nil;coreJVM/publishLocal;coreJS/publishLocal",
    dependencies = List(scalacheck)
  )

  lazy val disciplineMunit = SbtCommunityProject(
    project = "discipline-munit",
    sbtTestCommand = "test",
    sbtPublishCommand = s"""set every version := "${Versions.disciplineMunit}";coreJVM/publishLocal;coreJS/publishLocal""",
    dependencies = List(discipline, munit)
  )

end projects

def allProjects = projects.fields.of[CommunityProject].sortBy(_.project)

lazy val projectMap = allProjects.map(p => p.project -> p).toMap
