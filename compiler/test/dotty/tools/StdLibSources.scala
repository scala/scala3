package dotty.tools

import java.io.File
import scala.io.Source
import org.junit.Test
import org.junit.Assert._

object StdLibSources {

  /* For debug only */
  private val useExplicitWhiteList = false

  private final val stdLibPath = "../scala-library/src/library/"

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

class StdLibSources {
  @Test def checkWBLists = {
    val stdlibFilesBlackListed = StdLibSources.blacklisted

    val duplicates = stdlibFilesBlackListed.groupBy(x => x).filter(_._2.size > 1).filter(_._2.size > 1)
    val msg = duplicates.map(x => s"'${x._1}' appears ${x._2.size} times").mkString(s"Duplicate entries in ${StdLibSources.blacklistFile}:\n", "\n", "\n")
    assertTrue(msg, duplicates.isEmpty)

    val filesNotInStdLib = stdlibFilesBlackListed.toSet -- StdLibSources.all
    val msg2 = filesNotInStdLib.map(x => s"'$x'").mkString(s"Entries in ${StdLibSources.blacklistFile} where not found:\n", "\n", "\n")
    assertTrue(msg2, filesNotInStdLib.isEmpty)
  }
}
