// Usage
// > scala-cli project/scripts/dottyCompileBisect.scala -- [--run <main.class.name>] [<compiler-option> ...] <file1.scala> [<fileN.scala> ...]
//
// This script will bisect the compilation failure starting with a fast bisection on released nightly builds.
// Then it will bisect the commits between the last nightly that worked and the first nightly that failed.


import sys.process._
import scala.io.Source
import Releases.Release
import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}

@main def dottyCompileBisect(args: String*): Unit =
  val (mainClass, compilerArgs) = args match
    case Seq("--run", mainClass, compilerArgs*) =>
      (Some(mainClass), compilerArgs)
    case _ =>
      (None, args)

  val releaseBisect = ReleaseBisect(mainClass, compilerArgs.toList)
  val bisectedBadRelease = releaseBisect.bisectedBadRelease(Releases.allReleases)
  println("\nFinished bisecting releases\n")

  bisectedBadRelease match
    case Some(firstBadRelease) =>
      firstBadRelease.previous match
        case Some(lastGoodRelease) =>
          println(s"Last good release: $lastGoodRelease")
          println(s"First bad release: $firstBadRelease")
          val commitBisect = CommitBisect(mainClass, compilerArgs.toList)
          commitBisect.bisect(lastGoodRelease.hash, firstBadRelease.hash)
        case None =>
          println(s"No good release found")
    case None =>
      println(s"No bad release found")

class ReleaseBisect(mainClass: Option[String], compilerArgs: List[String]):
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
    val testCommand = mainClass match
      case Some(className) =>
        s"run --main-class '$className'"
      case None =>
        "compile"
    val res = s"""scala-cli $testCommand -S '${release.version}' ${compilerArgs.mkString(" ")}""".!
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

class CommitBisect(mainClass: Option[String], compilerArgs: List[String]):
  def bisect(lastGoodHash: String, fistBadHash: String): Unit =
    println(s"Starting bisecting commits $lastGoodHash..$fistBadHash\n")
    val runOption = mainClass.map(className => s"--run $className").getOrElse("")
    val scriptFile = Paths.get("project", "scripts", "dottyCompileBisect.sh")
    val tempScriptFile = File.createTempFile("dottyCompileBisect", "sh").toPath
    Files.copy(scriptFile, tempScriptFile, StandardCopyOption.REPLACE_EXISTING)
    "git bisect start".!
    s"git bisect bad $fistBadHash".!
    s"git bisect good $lastGoodHash".!
    s"git bisect run sh ${tempScriptFile.toAbsolutePath} ${runOption} ${compilerArgs.mkString(" ")}".!
