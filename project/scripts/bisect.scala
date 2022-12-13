/*
This script will bisect a problem with the compiler based on success/failure of the validation script passed as an argument.
It starts with a fast bisection on released nightly builds.
Then it will bisect the commits between the last nightly that worked and the first nightly that failed.
Look at the `usageMessage` below for more details.
*/


import sys.process._
import scala.io.Source
import Releases.Release
import java.io.File

val usageMessage = """
  |Usage:
  |  > scala-cli project/scripts/bisect.scala -- <validation-script-path> [versions-range]
  |
  |The validation script should be executable and accept a single parameter, which will be the scala version to validate.
  |Look at bisect-cli-example.sh and bisect-expect-example.exp for reference.
  |The optional versions range specifies which releases should be taken into account while bisecting.
  |The range format is <first>...<last>, where both <first> and <last> are optional, e.g.
  |* 3.1.0-RC1-bin-20210827-427d313-NIGHTLY..3.2.1-RC1-bin-20220716-bb9c8ff-NIGHTLY
  |* 3.2.1-RC1-bin-20220620-de3a82c-NIGHTLY..
  |* ..3.3.0-RC1-bin-20221124-e25362d-NIGHTLY
  |The ranges are treated as inclusive.
  |
  |Don't use the example scripts modified in place as they might disappear from the repo during a checkout.
  |Instead copy them to a different location first.
  |
  |Warning: The bisect script should not be run multiple times in parallel because of a potential race condition while publishing artifacts locally.
  |
  |Tip: Before running the bisect script run the validation script manually with some published versions of the compiler to make sure it succeeds and fails as expected.
""".stripMargin

@main def dottyCompileBisect(args: String*): Unit =
  val (validationScriptRawPath, versionsRange) = args match
    case Seq(path) =>
      (path, VersionsRange.all)
    case Seq(path, ParsedVersionsRange(range)) =>
      (path, range)
    case _ =>
      println("Wrong script parameters.")
      println()
      println(usageMessage)
      System.exit(1)
      null

  val validationScriptPath = (new File(validationScriptRawPath)).getAbsolutePath.toString
  given releases: Releases = Releases.fromRange(versionsRange)

  val releaseBisect = ReleaseBisect(validationScriptPath)
  val bisectedBadRelease = releaseBisect.bisectedBadRelease(releases.releases)
  println("\nFinished bisecting releases\n")

  bisectedBadRelease match
    case Some(firstBadRelease) =>
      firstBadRelease.previous match
        case Some(lastGoodRelease) =>
          println(s"Last good release: $lastGoodRelease")
          println(s"First bad release: $firstBadRelease")
          val commitBisect = CommitBisect(validationScriptPath)
          commitBisect.bisect(lastGoodRelease.hash, firstBadRelease.hash)
        case None =>
          println(s"No good release found")
    case None =>
      println(s"No bad release found")

case class VersionsRange(first: Option[String], last: Option[String]):
  def filter(versions: Seq[String]) =
    def versionIndex(version: String) =
      val lastMatchingNightly =
        if version.contains("-bin-") then version else
          versions.filter(_.startsWith(version)).last
      versions.indexOf(lastMatchingNightly)

    val startIdx = first.map(versionIndex(_)).getOrElse(0)
    assert(startIdx >= 0, s"${first} is not a nightly compiler release")
    val endIdx = last.map(versionIndex(_) + 1).getOrElse(versions.length)
    assert(endIdx > 0, s"${endIdx} is not a nightly compiler release")
    val filtered = versions.slice(startIdx, endIdx).toVector
    assert(filtered.nonEmpty, "No matching releases")
    filtered


object VersionsRange:
  def all = VersionsRange(None, None)

object ParsedVersionsRange:
  def unapply(range: String): Option[VersionsRange] = range match
    case s"${first}...${last}" => Some(VersionsRange(
      Some(first).filter(_.nonEmpty),
      Some(last).filter(_.nonEmpty)
    ))
    case _ => None

class ReleaseBisect(validationScriptPath: String):
  def bisectedBadRelease(releases: Vector[Release]): Option[Release] =
    Some(bisect(releases))
      .filter(!isGoodRelease(_))

  def bisect(releases: Vector[Release]): Release =
    assert(releases.length > 1, "Need at least 2 releases to bisect")
    if releases.length == 2 then
      if isGoodRelease(releases.head) then releases.last
      else releases.head
    else
      val mid = releases(releases.length / 2)
      if isGoodRelease(mid) then bisect(releases.drop(releases.length / 2))
      else bisect(releases.take(releases.length / 2 + 1))

  private def isGoodRelease(release: Release): Boolean =
    println(s"Testing ${release.version}")
    val result = Seq(validationScriptPath, release.version).!
    val isGood = result == 0
    println(s"Test result: ${release.version} is a ${if isGood then "good" else "bad"} release\n")
    isGood

class Releases(val releases: Vector[Release])

object Releases:
  private lazy val allReleases: Vector[String] =
    val re = raw"""(?<=title=")(.+-bin-\d{8}-\w{7}-NIGHTLY)(?=/")""".r
    val html = Source.fromURL("https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/")
    re.findAllIn(html.mkString).toVector

  def fromRange(range: VersionsRange): Releases = Releases(range.filter(allReleases).map(Release(_)))

  case class Release(version: String):
    private val re = raw".+-bin-(\d{8})-(\w{7})-NIGHTLY".r
    def date: String =
      version match
        case re(date, _) => date
        case _ => sys.error(s"Could not extract date from version $version")
    def hash: String =
      version match
        case re(_, hash) => hash
        case _ => sys.error(s"Could not extract hash from version $version")

    def previous(using r: Releases): Option[Release] =
      val idx = r.releases.indexOf(this)
      if idx == 0 then None
      else Some(r.releases(idx - 1))

    override def toString: String = version

class CommitBisect(validationScriptPath: String):
  def bisect(lastGoodHash: String, fistBadHash: String): Unit =
    println(s"Starting bisecting commits $lastGoodHash..$fistBadHash\n")
    val bisectRunScript = s"""
      |scalaVersion=$$(sbt "print scala3-compiler-bootstrapped/version" | tail -n1)
      |rm -r out
      |sbt "clean; scala3-bootstrapped/publishLocal"
      |$validationScriptPath "$$scalaVersion"
    """.stripMargin
    "git bisect start".!
    s"git bisect bad $fistBadHash".!
    s"git bisect good $lastGoodHash".!
    Seq("git", "bisect", "run", "sh", "-c", bisectRunScript).!
    s"git bisect reset".!
