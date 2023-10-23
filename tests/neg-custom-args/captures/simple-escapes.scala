class FileOutputStream(str: String):
  def close(): Unit = ()
  def write(x: Int): Unit = ()

def Test1 =

  def usingLogFile[T](op: (local: caps.Cap) -> FileOutputStream^{local} => T): T =
    val logFile = FileOutputStream("log")
    val result = op(caps.cap)(logFile)
    logFile.close()
    result

  var foo: FileOutputStream^{cap[Test1]} = FileOutputStream("")

  val later1 = usingLogFile { local => f =>
    foo = f      // error
    () => ()
  }
  val later2 = usingLogFile { local => f => // error
    () => f.write(0)
  }
  later1()
  later2()

