package dotty.tools

import dotty.tools.ListOfSources.loadList

object FromTastySources {

  // pos tests lists

  def posFromTastyBlacklistFile: String = "compiler/test/dotc/pos-from-tasty.blacklist"
  def posDecompilationBlacklistFile: String = "compiler/test/dotc/pos-decompilation.blacklist"
  def posRecompilationWhitelistFile: String = "compiler/test/dotc/pos-recompilation.whitelist"

  def posFromTastyBlacklisted: List[String] = loadList(posFromTastyBlacklistFile)
  def posDecompilationBlacklisted: List[String] = loadList(posDecompilationBlacklistFile)
  def posRecompilationWhitelist: List[String] = loadList(posRecompilationWhitelistFile)

  // run tests lists

  def runFromTastyBlacklistFile: String = "compiler/test/dotc/run-from-tasty.blacklist"
  def runDecompilationBlacklistFile: String = "compiler/test/dotc/run-decompilation.blacklist"
  def runRecompilationWhitelistFile: String = "compiler/test/dotc/run-recompilation.whitelist"

  def runFromTastyBlacklisted: List[String] = loadList(runFromTastyBlacklistFile)
  def runDecompilationBlacklisted: List[String] = loadList(runDecompilationBlacklistFile)
  def runRecompilationWhitelist: List[String] = loadList(runRecompilationWhitelistFile)
}
