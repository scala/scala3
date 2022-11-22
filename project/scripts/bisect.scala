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
  |  > scala-cli project/scripts/bisect.scala -- <validation-script>
  |
  |The validation script should be executable and accept a single parameter, which will be the scala version to validate.
  |Look at bisect-cli-example.sh and bisect-expect-example.exp for reference.
  |Don't use the example scripts modified in place as they might disappear from the repo during a checkout.
  |Instead copy them to a different location first.
  |
  |Warning: The bisect script should not be run multiple times in parallel because of a potential race condition while publishing artifacts locally.
  |
  |Tip: Before running the bisect script run the validation script manually with some published versions of the compiler to make sure it succeeds and fails as expected.
""".stripMargin

@main def dottyCompileBisect(args: String*): Unit =
  val validationScriptPath = args match
    case Seq(path) =>
      (new File(path)).getAbsolutePath.toString
    case _ =>
      println("Wrong script parameters.")
      println()
      println(usageMessage)
      System.exit(1)
      null

  val releaseBisect = ReleaseBisect(validationScriptPath)
  val bisectedBadRelease = releaseBisect.bisectedBadRelease(Releases.allReleases)
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

class ReleaseBisect(validationScriptPath: String):
  def bisectedBadRelease(releases: Vector[Release]): Option[Release] =
    Some(bisect(releases: Vector[Release]))
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

object Releases:
  lazy val allReleases: Vector[Release] =
    val re = raw"(?<=title=$")(.+-bin-\d{8}-\w{7}-NIGHTLY)(?=/$")".r
    val html = Source.fromURL("https://repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/")
    re.findAllIn(html.mkString).map(Release.apply).toVector

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

    def previous: Option[Release] =
      val idx = allReleases.indexOf(this)
      if idx == 0 then None
      else Some(allReleases(idx - 1))

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
