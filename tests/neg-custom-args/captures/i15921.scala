trait Stream { def close(): Unit = (); def write(x: Any): Unit = () }

object Test {
  def usingLogFile[T](op: (c: Stream^) => T): T =
    val logFile = new Stream { }
    val result = op(logFile)
    logFile.close()
    result

  val later = usingLogFile { f => () => f.write(0) } // error
  later()   // writing to closed file!
}
