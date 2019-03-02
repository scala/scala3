package dotty.tools

import java.io.File

import scala.io.Source

object TestSources {

  // Std Lib

  private final val stdLibPath = "tests/scala2-library/src/library/"

  private def blacklistFile: String = "compiler/test/dotc/scala-collections.blacklist"

  def stdLibWhitelisted: List[String] = all.diff(stdLibBlacklisted)
  def stdLibBlacklisted: List[String] = loadList(blacklistFile).map(stdLibPath + _)

  private def all: List[String] = {
    def collectAllFilesInDir(dir: File, acc: List[String]): List[String] = {
      val files = dir.listFiles()
      val acc2 = files.foldLeft(acc)((acc1, file) => if (file.isFile && file.getPath.endsWith(".scala")) file.getPath :: acc1 else acc1)
      files.foldLeft(acc2)((acc3, file) => if (file.isDirectory) collectAllFilesInDir(file, acc3) else acc3)
    }
    collectAllFilesInDir(new File(stdLibPath), Nil).sorted
  }

  // pos tests lists

  def posFromTastyBlacklistFile: String = "compiler/test/dotc/pos-from-tasty.blacklist"
  def posDecompilationBlacklistFile: String = "compiler/test/dotc/pos-decompilation.blacklist"
  def posRecompilationWhitelistFile: String = "compiler/test/dotc/pos-recompilation.whitelist"
  def posTestPicklingBlacklistFile: String = "compiler/test/dotc/pos-test-pickling.blacklist"

  def posFromTastyBlacklisted: List[String] = loadList(posFromTastyBlacklistFile)
  def posDecompilationBlacklisted: List[String] = loadList(posDecompilationBlacklistFile)
  def posRecompilationWhitelist: List[String] = loadList(posRecompilationWhitelistFile)
  def posTestPicklingBlacklisted: List[String] = loadList(posTestPicklingBlacklistFile)

  // run tests lists

  def runFromTastyBlacklistFile: String = "compiler/test/dotc/run-from-tasty.blacklist"
  def runDecompilationBlacklistFile: String = "compiler/test/dotc/run-decompilation.blacklist"
  def runRecompilationWhitelistFile: String = "compiler/test/dotc/run-recompilation.whitelist"
  def runTestPicklingBlacklistFile: String = "compiler/test/dotc/run-test-pickling.blacklist"

  def runFromTastyBlacklisted: List[String] = loadList(runFromTastyBlacklistFile)
  def runDecompilationBlacklisted: List[String] = loadList(runDecompilationBlacklistFile)
  def runRecompilationWhitelist: List[String] = loadList(runRecompilationWhitelistFile)
  def runTestPicklingBlacklisted: List[String] = loadList(runTestPicklingBlacklistFile)

  // load lists

  private def loadList(path: String): List[String] = Source.fromFile(path, "UTF8").getLines()
    .map(_.trim) // allow identation
    .filter(!_.startsWith("#")) // allow comment lines prefixed by #
    .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
    .filter(_.nonEmpty)
    .toList

}
