// Usage
// > scala-cli project/scripts/dottyCompileBisect.scala -- File.scala
//
// This script will bisect the compilation failure starting with a fast bisection on released nightly builds.
// Then it will bisect the commits between the last nightly that worked and the first nightly that failed.


import sys.process._
import scala.io.Source
import Releases.Release

@main def dottyCompileBisect(file: String): Unit =
  val releaseBisect = ReleaseBisect(file)
  val fistBadRelease = releaseBisect.bisect(Releases.allReleases)
  println("\nFinished bisecting releases\n")
  fistBadRelease.previous match
    case Some(lastGoodRelease) =>
      println(s"Last good release: $lastGoodRelease\nFirst bad release: $fistBadRelease\n")
      val commitBisect = CommitBisect(file)
      commitBisect.bisect(lastGoodRelease.hash, fistBadRelease.hash)
    case None =>
      println(s"No good release found")

class ReleaseBisect(file: String):

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
    val res = s"""scala-cli compile $file -S "${release.version}"""".!
    val isGood = res == 0
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

class CommitBisect(file: String):
  def bisect(lastGoodHash: String, fistBadHash: String): Unit =
    println(s"Starting bisecting commits $lastGoodHash..$fistBadHash\n")
    "git bisect start".!
    s"git bisect bad $fistBadHash".!
    s"git bisect good $lastGoodHash".!
    s"git bisect run sh project/scripts/dottyCompileBisect.sh $file".!
