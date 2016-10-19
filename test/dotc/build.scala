package dotc

import java.io.File

object build extends tests {

  private def deleteFilesInFolder(folder: File, deleteFolder: Boolean = false): Unit = {
    val files = folder.listFiles()
    if(files != null) { //some JVMs return null for empty dirs
        for(f <- files) {
            if(f.isDirectory) {
              deleteFilesInFolder(f, deleteFolder = true)
            } else {
                f.delete()
            }
        }
    }
    if(deleteFolder) folder.delete()
  }

  def clearOutput() = {
    deleteFilesInFolder(new File(defaultOutputDir)) // clear previous output
    val keepFile = new File(defaultOutputDir + ".keep")
    keepFile.createNewFile()
  }

  def main(args: Array[String]): Unit = {
    println("----------  Building bootstrapped dotty-lib  ----------------------------------------------")
    clearOutput()
    dottyBootedLib
    val p1 = Runtime.getRuntime.exec(Array("jar", "cf", "dotty-lib.jar", "-C", "out", "."))
    p1.waitFor()

    println("----------  Building bootstrapped dotty depending on dotty-lib compiled by dotty ----------")
    clearOutput()
    dottyDependsOnBootedLib
    val p2 = Runtime.getRuntime.exec(Array("jar", "cf", "dotty.jar", "-C", "out", "."))
    p2.waitFor()
  }
}
