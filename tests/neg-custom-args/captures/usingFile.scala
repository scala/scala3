// Reported in issue #17517

import language.experimental.captureChecking
import java.io.*

object Test:
  class Logger(f: OutputStream^):
    def log(msg: String): Unit = ???

  def usingFile[T](name: String, op: OutputStream^ => T): T =
    val f = new FileOutputStream(name)
    val result = op(f)
    f.close()
    result

  def usingLogger[T](f: OutputStream^)(op: Logger^{f} => T): T = ???

  usingFile( // error
    "foo",
    file => {
      usingLogger(file)(l => () => l.log("test"))
    }
  )
