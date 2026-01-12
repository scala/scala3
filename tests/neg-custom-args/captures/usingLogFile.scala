import java.io.*

object Test1:

  def usingLogFile[T](op: (local: caps.Capability) ?-> FileOutputStream => T): T =
    val logFile = FileOutputStream("log")
    val result = op(using caps.any)(logFile)
    logFile.close()
    result

  private val later = usingLogFile { f => () => f.write(0) }  // error
  later()

object Test2:

  def usingLogFile[T](op: FileOutputStream^ => T): T =
    val logFile = FileOutputStream("log")
    val result = op(logFile)
    logFile.close()
    result

  val later = usingLogFile { f => () => f.write(0) } // error
  later()

  class Cell[+T](val x: T)

  private val later2 = usingLogFile { f => Cell(() => f.write(0)) } // error
  later2.x()

object Test3:
  class Logger(f: OutputStream^):
    def log(msg: String): Unit = ???

  def usingFile[T](name: String, op: OutputStream^ => T): T =
    val f = new FileOutputStream(name)
    val result = op(f)
    f.close()
    result

  val xs: List[Int] = ???
  def good = usingFile("out", f => xs.foreach(x => f.write(x)))
  def fail =
    val later = usingFile("out", f => (y: Int) => xs.foreach(x => f.write(x + y))) // error
    later(1)

  def usingLogger[T](f: OutputStream^, op: Logger^{f} => T): T =
    val logger = Logger(f)
    op(logger)

  def test =
    val later = usingFile("logfile", // now ok
      usingLogger(_, l => () => l.log("test")))  // error after checking mapping scheme
    later()
