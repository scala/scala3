import java.io.*
object Test2:

  def usingLogFile[sealed T](op: FileOutputStream^ => T): T =
    val logFile = FileOutputStream("log")
    val result = op(logFile)
    logFile.close()
    result

  private val later = usingLogFile { f => () => f.write(0) } // error
