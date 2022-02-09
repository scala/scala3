import java.io.FileOutputStream
import annotation.capability

object Test1:

  def usingLogFile[T](op: FileOutputStream => T): T =
    val logFile = FileOutputStream("log")
    val result = op(logFile)
    logFile.close()
    result

  val later = usingLogFile { f => () => f.write(0) }
  later()

object Test2:

  def usingLogFile[T](op: ({*} FileOutputStream) => T): T =
    val logFile = FileOutputStream("log")
    val result = op(logFile)
    logFile.close()
    result

  val later = usingLogFile { f => () => f.write(0) } // error
  later()

  class Cell[+T](val x: T)

  val later2 = usingLogFile { f => Cell(() => f.write(0)) }
  later2.x()  // error

  var later3: () => Unit = () => ()  // error
  usingLogFile { f => later3 = () => f.write(0) }
  later3()

  var later4: Cell[() => Unit] = Cell(() => ())
  usingLogFile { f => later4 = Cell(() => f.write(0)) }
  later4.x() // error

