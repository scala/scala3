import java.io.FileOutputStream
import annotation.capability

def withLogFile1[T](op: FileOutputStream => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

def withLogFile2[T](op: ({*} FileOutputStream) => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

def test =
  val later1 = withLogFile1 { f => // ok
    () => f.write(0)
  }
  later1()

  val later2 = withLogFile2 { f => // error
    () => f.write(0)
  }
  later2()
