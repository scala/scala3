//> using jvm 17
/*
This script will bisect a problem with the compiler based on success/failure of the validation script passed as an argument.
It starts with a fast bisection on released nightly builds.
Then it will bisect the commits between the last nightly that worked and the first nightly that failed.
Look at the `usageMessage` below for more details.
*/


import sys.process._
import scala.io.Source
import java.io.File
import java.nio.file.attribute.PosixFilePermissions
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Using

val usageMessage = """
  |Usage:
  |  > scala-cli project/scripts/bisect.scala -- [<bisect-options>] <validation-command>
  |
  |The <validation-command> should be one of:
  |* compile <arg1> <arg2> ...
  |* run <arg1> <arg2> ...
  |* test <arg1> <arg2> ...
  |* <custom-validation-script-path>
  |
  |The arguments for 'compile' and 'run' should be paths to the source file(s) and optionally additional options passed directly to scala-cli.
  |
  |A custom validation script should be executable and accept a single parameter, which will be the scala version to validate.
  |Look at bisect-cli-example.sh and bisect-expect-example.exp for reference.
  |If you want to use one of the example scripts - use a copy of the file instead of modifying it in place because that might mess up the checkout.
  |
  |The optional <bisect-options> may be any combination of:
  |Boolean flags accept an optional =true|false value; the bare form means true.
  |
  |* --dry-run[=true|false]
  |    Don't try to bisect - just make sure the validation command works correctly
  |
  |* --releases <releases-range>
  |    Bisect only releases from the given range (defaults to all releases).
  |    The range format is <first>...<last>, where both <first> and <last> are optional, e.g.
  |    * 3.1.0-RC1-bin-20210827-427d313-NIGHTLY...3.2.1-RC1-bin-20220716-bb9c8ff-NIGHTLY
  |    * 3.2.1-RC1-bin-20220620-de3a82c-NIGHTLY...
  |    * ...3.3.0-RC1-bin-20221124-e25362d-NIGHTLY
  |    The ranges are treated as inclusive.
  |
  |* --bootstrapped[=true|false]
  |    Publish locally and test a bootstrapped compiler rather than a nonbootstrapped one.
  |
  |* --should-fail[=true|false]
  |    Expect the validation command to fail rather that succeed. This can be used e.g. to find out when some illegal code started to compile.
  |
  |* --with-bloop[=true|false]
  |    Use Bloop/build server for scala-cli reproduction scripts (omit --server=false).
  |    Implies --with-cleaning, because incremental compilation may interfere with bisection.
  |
  |* --with-cleaning[=true|false]
  |    Run `scala-cli clean <input>` before each validation invocation.
  |    Enabled by default when --with-bloop is set; pass --with-cleaning=false to disable it even with --with-bloop.
  |
  |Warning: The bisect script should not be run multiple times in parallel because of a potential race condition while publishing artifacts locally.

""".stripMargin

@main def run(args: String*): Unit =
  val scriptOptions =
    try ScriptOptions.fromArgs(args)
    catch
      case _ =>
        sys.error(s"Wrong script parameters.\n${usageMessage}")

  val validationScript = scriptOptions.validationCommand.validationScript(
    withBloop = scriptOptions.withBloop,
    withCleaning = scriptOptions.withCleaning
  )
  val releases = Releases.fromRange(scriptOptions.releasesRange)
  val releaseBisect = ReleaseBisect(validationScript, shouldFail = scriptOptions.shouldFail, releases)

  releaseBisect.verifyEdgeReleases()

  if (!scriptOptions.dryRun) then
    val (lastGoodRelease, firstBadRelease) = releaseBisect.bisectedGoodAndBadReleases()
    println(s"Last good release: ${lastGoodRelease.version}")
    println(s"First bad release: ${firstBadRelease.version}")
    println("\nFinished bisecting releases\n")

    val commitBisect = CommitBisect(validationScript, shouldFail = scriptOptions.shouldFail, bootstrapped = scriptOptions.bootstrapped, lastGoodRelease.hash, firstBadRelease.hash)
    commitBisect.bisect()


case class ScriptOptions(
    validationCommand: ValidationCommand,
    dryRun: Boolean,
    bootstrapped: Boolean,
    releasesRange: ReleasesRange,
    shouldFail: Boolean,
    withBloop: Boolean,
    withCleaning: Boolean,
    withCleaningExplicit: Option[Boolean] = None
)
object ScriptOptions:
  private object BooleanFlag:
    def unapply(arg: String): Option[(String, Boolean)] =
      arg match
        case name @ ("--dry-run" | "--bootstrapped" | "--should-fail" | "--with-bloop" | "--with-cleaning") =>
          Some((name, true))
        case s"$name=$value" if name == "--dry-run" || name == "--bootstrapped" || name == "--should-fail" || name == "--with-bloop" || name == "--with-cleaning" =>
          Some((name, value.toBooleanOption.getOrElse(sys.error(s"Invalid boolean value for $name: $value"))))
        case _ => None

  def fromArgs(args: Seq[String]) =
    val defaultOptions = ScriptOptions(
      validationCommand = null,
      dryRun = false,
      bootstrapped = false,
      ReleasesRange(first = None, last = None),
      shouldFail = false,
      withBloop = false,
      withCleaning = false
    )
    val options = parseArgs(args, defaultOptions)
    val resolvedCleaning = options.withCleaningExplicit match
      case Some(value) => value
      case None =>
        if options.withBloop then
          println("--with-bloop implies --with-cleaning: enabling cleaning because incremental compilation may interfere with bisection")
          true
        else false
    options.copy(withCleaning = resolvedCleaning)

  private def parseArgs(args: Seq[String], options: ScriptOptions): ScriptOptions =
    args.toList match
      case arg :: argsRest =>
        BooleanFlag.unapply(arg) match
          case Some((name, value)) =>
            name match
              case "--dry-run" => parseArgs(argsRest, options.copy(dryRun = value))
              case "--bootstrapped" => parseArgs(argsRest, options.copy(bootstrapped = value))
              case "--should-fail" => parseArgs(argsRest, options.copy(shouldFail = value))
              case "--with-bloop" => parseArgs(argsRest, options.copy(withBloop = value))
              case "--with-cleaning" => parseArgs(argsRest, options.copy(withCleaningExplicit = Some(value)))
              case other => sys.error(s"Unexpected boolean flag: $other")
          case None =>
            arg match
              case "--releases" =>
                val range = ReleasesRange.tryParse(argsRest.head).get
                parseArgs(argsRest.tail, options.copy(releasesRange = range))
              case _ if options.validationCommand == null =>
                val command = ValidationCommand.fromArgs(args)
                options.copy(validationCommand = command)
              case _ =>
                options
      case Nil =>
        options

enum ValidationCommand:
  case Compile(args: Seq[String])
  case Run(args: Seq[String])
  case Test(args: Seq[String])
  case CustomValidationScript(scriptFile: File)

  def validationScript(withBloop: Boolean, withCleaning: Boolean): File = this match
    case Compile(args) => ValidationScript.tmpScalaCliScript(command = "compile", args, withBloop, withCleaning)
    case Run(args) => ValidationScript.tmpScalaCliScript(command = "run", args, withBloop, withCleaning)
    case Test(args) => ValidationScript.tmpScalaCliScript(command = "test", args, withBloop, withCleaning)
    case CustomValidationScript(scriptFile) => ValidationScript.copiedFrom(scriptFile)

object ValidationCommand:
  def fromArgs(args: Seq[String]) = args match
    case Seq("compile", commandArgs*) => Compile(commandArgs)
    case Seq("run", commandArgs*) => Run(commandArgs)
    case Seq("test", commandArgs*) => Test(commandArgs)
    case Seq(path) => CustomValidationScript(new File(path))


object ValidationScript:
  def copiedFrom(file: File): File =
    val fileContent = scala.io.Source.fromFile(file).mkString
    tmpScript(fileContent)

  def tmpScalaCliScript(command: String, args: Seq[String], withBloop: Boolean, withCleaning: Boolean): File =
    val argsString = args.mkString(" ")
    val serverModifier = if withBloop then "" else "--server=false "
    val cleanCommand = if withCleaning then s"scala-cli clean ${argsString}" else ""
    tmpScript(s"""
      |#!/usr/bin/env bash
      |export JAVA_HOME=${sys.props("java.home")}
      |${cleanCommand}
      |scala-cli ${command} -S "$$1" ${serverModifier}${argsString}
      |""".stripMargin
    )

  private def tmpScript(content: String): File =
    val executableAttr = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwxr-xr-x"))
    val tmpPath = Files.createTempFile("scala-bisect-validator", "", executableAttr)
    val tmpFile = tmpPath.toFile

    print(s"Bisecting with validation script: ${tmpPath.toAbsolutePath}\n")
    print("#####################################\n")
    print(s"${content}\n\n")
    print("#####################################\n\n")

    tmpFile.deleteOnExit()
    Files.write(tmpPath, content.getBytes(StandardCharsets.UTF_8))
    tmpFile


case class ReleasesRange(first: Option[String], last: Option[String]):
  def filter(releases: Seq[Release]) =
    def releaseIndex(version: String): Int =
      val index = releases.indexWhere(_.version == version)
      assert(index >= 0, s"${version} matches no nightly compiler release")
      index
    val startIdx = first.map(releaseIndex(_)).getOrElse(0)
    val endIdx = last.map(releaseIndex(_) + 1).getOrElse(releases.length)
    val filtered = releases.slice(startIdx, endIdx).toVector
    assert(filtered.nonEmpty, "No matching releases")
    filtered

object ReleasesRange:
  def all = ReleasesRange(None, None)
  def tryParse(range: String): Option[ReleasesRange] = range match
    case s"${first}...${last}" => Some(ReleasesRange(
      Some(first).filter(_.nonEmpty),
      Some(last).filter(_.nonEmpty)
    ))
    case _ => None

class Releases(val releases: Vector[Release])

object Releases:
  /* Nightlies used to be published to Maven Central and are published to the Artifactory nightlies
   * repository since 2025-08. Neither repository holds the full history on its own, so both are queried. */
  private val metadataUrls = Seq(
    "https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/maven-metadata.xml",
    "https://repo.scala-lang.org/artifactory/maven-nightlies/org/scala-lang/scala3-compiler_3/maven-metadata.xml"
  )

  lazy val allReleases: Vector[Release] =
    val re = raw"<version>(.+-bin-\d{8}-\w{7}-NIGHTLY)</version>".r
    metadataUrls
      .flatMap: url =>
        val xml =
          try Using.resource(Source.fromURL(url))(_.mkString)
          catch case ex: Exception => sys.error(s"Could not fetch the list of nightly releases from ${url}: ${ex}")
        re.findAllMatchIn(xml).map(_.group(1))
      .distinct
      .map(Release.apply)
      .toVector
      .sortBy: release =>
        (release.semanticVersion, release.date)

  def fromRange(range: ReleasesRange): Vector[Release] = range.filter(allReleases)

case class Release(version: String):
  private val re = raw".+-bin-(\d{8})-(\w{7})-NIGHTLY".r
  val semanticVersion: (major: Int, minor: Int, patch: Int) = version match 
    case s"$major.$minor.$patch-$_" => (major.toInt, minor.toInt, patch.toInt)
    case s"$major.$minor.$patch" => (major.toInt, minor.toInt, patch.toInt)
    case _ => sys.error(s"Could not extract semantic version from release name: $version")
    
  def date: LocalDate = LocalDate.parse(dateString, DateTimeFormatter.BASIC_ISO_DATE)
  def dateString: String =
    version match
      case re(date, _) => date
      case _ => sys.error(s"Could not extract date from release name: $version")
  def hash: String =
    version match
      case re(_, hash) => hash
      case _ => sys.error(s"Could not extract hash from release name: $version")

  override def toString: String = version


class ReleaseBisect(validationScript: File, shouldFail: Boolean, allReleases: Vector[Release]):
  assert(allReleases.length > 1, "Need at least 2 releases to bisect")

  private val isGoodReleaseCache = collection.mutable.Map.empty[Release, Boolean]

  def verifyEdgeReleases(): Unit =
    println(s"Verifying the first release: ${allReleases.head.version}")
    assert(isGoodRelease(allReleases.head), s"The evaluation script unexpectedly failed for the first checked release")
    println(s"Verifying the last release: ${allReleases.last.version}")
    assert(!isGoodRelease(allReleases.last), s"The evaluation script unexpectedly succeeded for the last checked release")

  def bisectedGoodAndBadReleases(): (Release, Release) =
    val firstBadRelease = bisect(allReleases)
    assert(!isGoodRelease(firstBadRelease), s"Bisection error: the 'first bad release' ${firstBadRelease.version} is not a bad release")
    val lastGoodRelease = firstBadRelease.previous
    assert(isGoodRelease(lastGoodRelease), s"Bisection error: the 'last good release' ${lastGoodRelease.version} is not a good release")
    (lastGoodRelease, firstBadRelease)

  extension (release: Release) private def previous: Release =
    val idx = allReleases.indexOf(release)
    allReleases(idx - 1)

  private def bisect(releases: Vector[Release]): Release =
    if releases.length == 2 then
      if isGoodRelease(releases.head) then releases.last
      else releases.head
    else
      val mid = releases(releases.length / 2)
      if isGoodRelease(mid) then bisect(releases.drop(releases.length / 2))
      else bisect(releases.take(releases.length / 2 + 1))

  private def isGoodRelease(release: Release): Boolean =
    isGoodReleaseCache.getOrElseUpdate(release, {
      println(s"Testing ${release.version}")
      val result = Seq(validationScript.getAbsolutePath, release.version).!
      val isGood = if(shouldFail) result != 0 else result == 0 // invert the process status if failure was expected
      println(s"Test result: ${release.version} is a ${if isGood then "good" else "bad"} release\n")
      isGood
    })

/** What the build-and-validate script should do when the compiler cannot be built. */
enum BuildFailureAction(val exitCode: Int):
  /** Tell `git bisect run` that this commit cannot be tested. */
  case SkipCommit extends BuildFailureAction(CommitBisectScripts.skipExitCode)
  /** Abort, because there is no bisection in progress that could skip the commit. */
  case AbortBisect extends BuildFailureAction(CommitBisectScripts.abortExitCode)

object CommitBisectScripts:
  /** `git bisect run` treats this status as "this commit cannot be tested". */
  val skipExitCode = 125
  /** `git bisect run` aborts on statuses of 128 and above instead of recording a verdict. */
  val abortExitCode = 128

  def sbtPublishRecipe(bootstrapped: Boolean): String =
    val scala3Project = if bootstrapped then "scala3-bootstrapped" else "scala3"
    Seq(
      "clean",
      """set every doc := new File("unused")""",
      s"set scaladoc/Compile/resourceGenerators := (`$scala3Project`/Compile/resourceGenerators).value",
      s"$scala3Project/publishLocal",
    ).mkString("; ")

  /** A script that publishes the compiler built from the currently checked out commit
   *  and validates it, exiting with a status that `git bisect run` understands.
   */
  def buildAndValidateScript(
      validationScriptPath: String,
      shouldFail: Boolean,
      bootstrapped: Boolean,
      onBuildFailure: BuildFailureAction
  ): String =
    val scala3CompilerProject = if bootstrapped then "scala3-compiler-bootstrapped" else "scala3-compiler"
    val validationCommandStatusModifier = if shouldFail then "! " else "" // invert the process status if failure was expected
    val publishRecipe = sbtPublishRecipe(bootstrapped)
    raw"""
      |commit=$$(git rev-parse --short HEAD)
      |echo "Testing commit $$commit"
      |scalaVersion=$$(sbt "print ${scala3CompilerProject}/version" | tr -d '\r' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+' | tail -n1)
      |if [ -z "$$scalaVersion" ]; then
      |  echo "Could not read the ${scala3CompilerProject} version at $$commit, aborting the bisection"
      |  exit ${abortExitCode}
      |fi
      |echo "Compiler version at $$commit: $$scalaVersion"
      |rm -rf out
      |export JAVA_HOME=${sys.props("java.home")}
      |sbt_build_log=$$(mktemp)
      |echo 'Running sbt publish recipe: sbt "$publishRecipe"'
      |if sbt '$publishRecipe' >"$$sbt_build_log" 2>&1; then
      |  rm -f "$$sbt_build_log"
      |  ${validationCommandStatusModifier}${validationScriptPath} "$$scalaVersion"
      |else
      |  echo "Failed to build the compiler at $$commit"
      |  cat "$$sbt_build_log"
      |  rm -f "$$sbt_build_log"
      |  exit ${onBuildFailure.exitCode}
      |fi
    """.stripMargin

class CommitBisect(validationScript: File, shouldFail: Boolean, bootstrapped: Boolean, lastGoodHash: String, firstBadHash: String):
  def bisect(): Unit =
    println(s"Starting bisecting commits $lastGoodHash..$firstBadHash\n")
    verifyEdgeCommits()

    val bisectRunScript = buildAndValidateScript(BuildFailureAction.SkipCommit)
    "git bisect start".!
    s"git bisect bad $firstBadHash".!
    s"git bisect good $lastGoodHash".!
    Seq("git", "bisect", "run", "sh", "-c", bisectRunScript).!
    s"git bisect reset".!

  private def buildAndValidateScript(onBuildFailure: BuildFailureAction): String =
    CommitBisectScripts.buildAndValidateScript(
      validationScript.getAbsolutePath,
      shouldFail = shouldFail,
      bootstrapped = bootstrapped,
      onBuildFailure = onBuildFailure
    )

  /** Build and validate both ends of the bisected range before bisecting it.
   *  The release bisection already established that the compiler changed its behaviour
   *  between them, so a verdict that disagrees means the validation is not measuring the
   *  locally built compiler, and bisecting would report an arbitrary commit as the culprit.
   */
  private def verifyEdgeCommits(): Unit =
    val originalRef = currentRef()
    try
      verifyEdgeCommit(lastGoodHash, expectedGood = true)
      verifyEdgeCommit(firstBadHash, expectedGood = false)
      println("Both edge commits behave as expected\n")
    finally
      println(s"Checking out $originalRef again")
      Seq("git", "checkout", originalRef).!

  private def verifyEdgeCommit(hash: String, expectedGood: Boolean): Unit =
    val expected = if expectedGood then "good" else "bad"
    println(s"Verifying the '$expected' commit $hash by building it and validating the result")
    if Seq("git", "checkout", "--detach", hash).! != 0 then
      sys.error(s"Could not check out $hash. Make sure the working tree is clean before bisecting.")

    val status = Seq("sh", "-c", buildAndValidateScript(BuildFailureAction.AbortBisect)).!
    if status == CommitBisectScripts.abortExitCode then
      sys.error(s"Could not build and validate the compiler at $hash, see the output above.")

    val actual = if status == 0 then "good" else "bad"
    println(s"Test result: $hash is a $actual commit\n")
    if actual != expected then
      sys.error(
        s"""|The validation command reported that $hash is a '$actual' commit,
            |but the release bisection determined that it is a '$expected' one. The validation command
            |is not measuring the compiler that was just built from that commit. Common causes are:
            |* the reproduction files are not present in the working tree after `git checkout`
            |  (keep them outside of the repository or in a gitignored directory like `local/`),
            |* the validation command fails for a reason unrelated to the compiler,
            |* the locally published compiler is not the one picked up by the validation command.
            |Run the validation command manually at $hash to see what happens.""".stripMargin
      )

  private def currentRef(): String =
    val branch = scala.util.Try(Process(Seq("git", "symbolic-ref", "--quiet", "--short", "HEAD")).!!.trim).toOption
    branch.filter(_.nonEmpty).getOrElse(Process(Seq("git", "rev-parse", "HEAD")).!!.trim)
