package dotty.tools

import scala.language.unsafeNulls

import java.io.File
import java.nio.file._

import scala.jdk.CollectionConverters._

object TestSources {

  // pos tests lists

  def posFromTastyBlacklistFile: String = "compiler/test/dotc/pos-from-tasty.blacklist"
  def posTestPicklingBlacklistFile: String = "compiler/test/dotc/pos-test-pickling.blacklist"
  def posTestRecheckExcludesFile: String = "compiler/test/dotc/pos-test-recheck.excludes"
  def posLazyValsAllowlistFile: String = "compiler/test/dotc/pos-lazy-vals-tests.allowlist"
  def posLintingAllowlistFile: String = "compiler/test/dotc/pos-linting.allowlist"

  def posFromTastyBlacklisted: List[String] = loadList(posFromTastyBlacklistFile)
  def posTestPicklingBlacklisted: List[String] = loadList(posTestPicklingBlacklistFile)
  def posTestRecheckExcluded: List[String] = loadList(posTestRecheckExcludesFile)
  def posLazyValsAllowlist: List[String] = loadList(posLazyValsAllowlistFile)
  def posLintingAllowlist: List[String] = loadList(posLintingAllowlistFile)

  // run tests lists

  def runFromTastyBlacklistFile: String = "compiler/test/dotc/run-from-tasty.blacklist"
  def runTestPicklingBlacklistFile: String = "compiler/test/dotc/run-test-pickling.blacklist"
  def runTestRecheckExcludesFile: String = "compiler/test/dotc/run-test-recheck.excludes"
  def runLazyValsAllowlistFile: String = "compiler/test/dotc/run-lazy-vals-tests.allowlist"


  def runFromTastyBlacklisted: List[String] = loadList(runFromTastyBlacklistFile)
  def runTestPicklingBlacklisted: List[String] = loadList(runTestPicklingBlacklistFile)
  def runTestRecheckExcluded: List[String] = loadList(runTestRecheckExcludesFile)
  def runLazyValsAllowlist: List[String] = loadList(runLazyValsAllowlistFile)

  // load lists

  private def loadList(path: String): List[String] = {
    val list = Files.readAllLines(Paths.get(path))
      .iterator()
      .asScala
      .map(_.trim)                     // allow indentation
      .filterNot(_.startsWith("#"))    // allow comment lines prefixed by #
      .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
      .filter(_.nonEmpty)
      .toList

    if (list.isEmpty)
      println(s"$path is empty")

    list
  }

  /** Retrieve sources from a directory */
  def sources(path: Path, excludedFiles: List[String] = Nil, shallow: Boolean = false): List[String] = {
    def fileFilter(path: Path) = {
      val fileName = path.getFileName.toString
      (fileName.endsWith(".scala") || fileName.endsWith(".java")) && !excludedFiles.contains(fileName)
    }

    assert(Files.isDirectory(path), s"Not a directory: $path")
    val files = if (shallow) Files.list(path) else Files.walk(path)
    try {
      val sources = files
        .filter(fileFilter)
        .sorted // make compilation order deterministic
        .iterator()
        .asScala
        .map(_.toString)
        .toList

      sources
    }
    finally files.close()
  }
}
