package dotty.tools

import scala.language.unsafeNulls

import java.io.File
import java.nio.file._

import scala.jdk.CollectionConverters._
import dotty.Properties

object TestSources {

  // pos tests lists

  def posFromTastyExcludelistFile: String = "compiler/test/dotc/pos-from-tasty.excludelist"
  def posTestPicklingExcludelistFile: String = "compiler/test/dotc/pos-test-pickling.excludelist"
  def posTestRecheckExcludesFile: String = "compiler/test/dotc/pos-test-recheck.excludes"
  def posLazyValsAllowlistFile: String = "compiler/test/dotc/pos-lazy-vals-tests.allowlist"
  def posLintingAllowlistFile: String = "compiler/test/dotc/pos-linting.allowlist"
  def posInitGlobalScala2LibraryTastyExcludelistFile: String = "compiler/test/dotc/pos-init-global-scala2-library-tasty.excludelist"

  def posFromTastyExcludelisted: List[String] = loadList(posFromTastyExcludelistFile)
  def posTestPicklingExcludelisted: List[String] = loadList(posTestPicklingExcludelistFile)
  def posTestRecheckExcluded: List[String] = loadList(posTestRecheckExcludesFile)
  def posLazyValsAllowlist: List[String] = loadList(posLazyValsAllowlistFile)
  def posLintingAllowlist: List[String] = loadList(posLintingAllowlistFile)
  def posInitGlobalScala2LibraryTastyExcludelisted: List[String] =
    if Properties.usingScalaLibraryTasty then loadList(posInitGlobalScala2LibraryTastyExcludelistFile)
    else Nil

  // run tests lists

  def runFromTastyExcludelistFile: String = "compiler/test/dotc/run-from-tasty.excludelist"
  def runTestPicklingExcludelistFile: String = "compiler/test/dotc/run-test-pickling.excludelist"
  def runTestRecheckExcludesFile: String = "compiler/test/dotc/run-test-recheck.excludes"
  def runLazyValsAllowlistFile: String = "compiler/test/dotc/run-lazy-vals-tests.allowlist"
  def runMacrosScala2LibraryTastyExcludelistFile: String = "compiler/test/dotc/run-macros-scala2-library-tasty.excludelist"

  def runFromTastyExcludelisted: List[String] = loadList(runFromTastyExcludelistFile)
  def runTestPicklingExcludelisted: List[String] = loadList(runTestPicklingExcludelistFile)
  def runTestRecheckExcluded: List[String] = loadList(runTestRecheckExcludesFile)
  def runLazyValsAllowlist: List[String] = loadList(runLazyValsAllowlistFile)
  def runMacrosScala2LibraryTastyExcludelisted: List[String] =
    if Properties.usingScalaLibraryTasty then loadList(runMacrosScala2LibraryTastyExcludelistFile)
    else Nil

  // neg tests lists

  def negScala2LibraryTastyExcludelistFile: String = "compiler/test/dotc/neg-scala2-library-tasty.excludelist"
  def negInitGlobalScala2LibraryTastyExcludelistFile: String = "compiler/test/dotc/neg-init-global-scala2-library-tasty.excludelist"
  def negExplicitNullsScala2LibraryTastyExcludelistFile: String = "compiler/test/dotc/neg-explicit-nulls-scala2-library-tasty.excludelist"

  def negScala2LibraryTastyExcludelisted: List[String] =
    if Properties.usingScalaLibraryTasty then loadList(negScala2LibraryTastyExcludelistFile)
    else Nil
  def negInitGlobalScala2LibraryTastyExcludelisted: List[String] =
    if Properties.usingScalaLibraryTasty then loadList(negInitGlobalScala2LibraryTastyExcludelistFile)
    else Nil
  def negExplicitNullsScala2LibraryTastyExcludelisted: List[String] =
    if Properties.usingScalaLibraryTasty then loadList(negExplicitNullsScala2LibraryTastyExcludelistFile)
    else Nil

  // patmat tests lists

  def patmatExhaustivityScala2LibraryTastyExcludelistFile: String = "compiler/test/dotc/patmat-exhaustivity-scala2-library-tasty.excludelist"

  def patmatExhaustivityScala2LibraryTastyExcludelisted: List[String] =
    if Properties.usingScalaLibraryTasty then loadList(patmatExhaustivityScala2LibraryTastyExcludelistFile)
    else Nil

  // neg best effort tests lists

  def negBestEffortPicklingExcludelistFile: String = "compiler/test/dotc/neg-best-effort-pickling.excludelist"
  def negBestEffortUnpicklingExcludelistFile: String = "compiler/test/dotc/neg-best-effort-unpickling.excludelist"

  def negBestEffortPicklingExcludelisted: List[String] = loadList(negBestEffortPicklingExcludelistFile)
  def negBestEffortUnpicklingExcludelisted: List[String] = loadList(negBestEffortUnpicklingExcludelistFile)

  // scoverage tests lists

  def scoverageIgnoreExcludelistFile: String = "compiler/test/dotc/scoverage-ignore.excludelist"

  def scoverageIgnoreExcludelisted: List[String] = loadList(scoverageIgnoreExcludelistFile)

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
