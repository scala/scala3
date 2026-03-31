object Test2:
  class File:
    def write(): Unit = ()
    def close(): Unit = ()
  class Box[+T](x: T)

  def usingLogFile[T](op: File^ => T): T =
    val logFile = File()
    val result = op(logFile)
    logFile.close()
    result


  private val later1 = usingLogFile { f => () => f.write() } // error
  private val later2 = usingLogFile { f => Box(f) } // error
  private val later3 = usingLogFile[() => Unit]: // error
    f => () => f.write() // error
  private val later4 = usingLogFile[Box[File^]]:
    f => Box(f) // error
