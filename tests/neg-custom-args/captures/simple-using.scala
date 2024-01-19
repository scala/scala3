import java.io.*
def usingLogFile[T](op: FileOutputStream^ => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result
def test() =
  usingLogFile { f => () => f.write(2) } // error
