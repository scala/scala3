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

  def main(args: Array[String]): Unit = {
    println("------------  Building dotty  ------------")
    deleteFilesInFolder(new File(defaultOutputDir)) // clear previous output
    val keepFile = new File(defaultOutputDir + ".keep")
    keepFile.createNewFile()
    dotty // build output dir
    val p = Runtime.getRuntime.exec(Array("jar", "cf", "dotty.jar", "-C", "out", "."))
    p.waitFor()
    p
  }
}
