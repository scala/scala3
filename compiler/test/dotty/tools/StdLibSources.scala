package dotty.tools

import java.io.File

import scala.io.Source

object StdLibSources {

  /* For debug only */
  private val useExplicitWhiteList = false

  private final val stdLibPath = "../scala-scala/src/library/"

  def blacklistFile: String = "../compiler/test/dotc/scala-collections.blacklist"
  private def whitelistFile: String = "../compiler/test/dotc/scala-collections.whitelist"

  def whitelisted: List[String] = {
    lazy val whitelistBasedOnBlacklist = all.diff(blacklisted)
    if (!useExplicitWhiteList) {
      whitelistBasedOnBlacklist
    } else if (!new File(whitelistFile).exists()) {
      genWhitelist(whitelistBasedOnBlacklist.map(_.replace(stdLibPath, "")))
      whitelistBasedOnBlacklist
    } else {
      loadList(whitelistFile)
    }
  }

  def blacklisted: List[String] = loadList(blacklistFile)

  def all: List[String] = {
    def collectAllFilesInDir(dir: File, acc: List[String]): List[String] = {
      val files = dir.listFiles()
      val acc2 = files.foldLeft(acc)((acc1, file) => if (file.isFile && file.getPath.endsWith(".scala")) file.getPath :: acc1 else acc1)
      files.foldLeft(acc2)((acc3, file) => if (file.isDirectory) collectAllFilesInDir(file, acc3) else acc3)
    }
    collectAllFilesInDir(new File(stdLibPath), Nil)
  }

  private def genWhitelist(list: List[String]): Unit = {
    println(s"Generating $whitelistFile based on $blacklistFile")
    val whitelist = new File(whitelistFile)
    val p = new java.io.PrintWriter(whitelist)
    try {
      list.foreach(p.println)
    } finally {
      p.close()
    }
  }

  private def loadList(path: String): List[String] = Source.fromFile(path, "UTF8").getLines()
    .map(_.trim) // allow identation
    .filter(!_.startsWith("#")) // allow comment lines prefixed by #
    .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
    .filter(_.nonEmpty)
    .map(stdLibPath + _)
    .toList

}
