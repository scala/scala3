package dotty.tools

import java.io.File

import dotty.tools.ListOfSources.loadList

object StdLibSources {

  private final val stdLibPath = "scala2-library/src/library/"

  def blacklistFile: String = "compiler/test/dotc/scala-collections.blacklist"

  def whitelisted: List[String] = all.diff(blacklisted)
  def blacklisted: List[String] = loadList(blacklistFile).map(stdLibPath + _)

  def all: List[String] = {
    def collectAllFilesInDir(dir: File, acc: List[String]): List[String] = {
      val files = dir.listFiles()
      val acc2 = files.foldLeft(acc)((acc1, file) => if (file.isFile && file.getPath.endsWith(".scala")) file.getPath :: acc1 else acc1)
      files.foldLeft(acc2)((acc3, file) => if (file.isDirectory) collectAllFilesInDir(file, acc3) else acc3)
    }
    collectAllFilesInDir(new File(stdLibPath), Nil)
  }

}
