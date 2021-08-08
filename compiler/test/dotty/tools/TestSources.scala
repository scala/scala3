package dotty.tools

import java.io.File
import java.nio.file._

import scala.collection.JavaConverters._

object TestSources {

  // pos tests lists

  def posFromTastyBlacklistFile: String = "compiler/test/dotc/pos-from-tasty.blacklist"
  def posTestPicklingBlacklistFile: String = "compiler/test/dotc/pos-test-pickling.blacklist"
  def posTestRecheckExcludesFile = "compiler/test/dotc/pos-test-recheck.exludes"

  def posFromTastyBlacklisted: List[String] = loadList(posFromTastyBlacklistFile)
  def posTestPicklingBlacklisted: List[String] = loadList(posTestPicklingBlacklistFile)
  def posTestRecheckExcluded = loadList(posTestRecheckExcludesFile)

  // run tests lists

  def runFromTastyBlacklistFile: String = "compiler/test/dotc/run-from-tasty.blacklist"
  def runTestPicklingBlacklistFile: String = "compiler/test/dotc/run-test-pickling.blacklist"
  def runTestRecheckExcludesFile = "compiler/test/dotc/run-test-recheck.exludes"

  def runFromTastyBlacklisted: List[String] = loadList(runFromTastyBlacklistFile)
  def runTestPicklingBlacklisted: List[String] = loadList(runTestPicklingBlacklistFile)
  def runTestRecheckExcluded = loadList(runTestRecheckExcludesFile)

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
