
import java.io.*
def Test2 =

  def usingLogFile[sealed T](op: FileOutputStream^ => T): T =
    val logFile = FileOutputStream("log")
    val result = op(logFile)
    logFile.close()
    result

  val later = usingLogFile { f => () => f.write(0) } // error
  val later2 = usingLogFile[(() => Unit) | Null] { f => () => f.write(0) } // error

  var x: (FileOutputStream^{cap[Test2]}) | Null = null
  def foo(f: FileOutputStream^, g: FileOutputStream^) =
    var y = if ??? then f else g  // ok
    val yc: FileOutputStream^{f,g} = y

  usingLogFile { f => x = f }  // error

  later()

def Test3 =
  def f[T](y: T) =
    var x: T = y
    ()

  class C[T](y: T):
    object o:
      var x: T = y    // error
      ()

  class C2[T](y: T):
    def f =
      var x: T = y    // ok
      ()

  def g1[T](y: T): T => Unit =
    var x: T = y     // error
    y => x = y

  def g2[T](y: T): T => Unit =
    var x: T = y     // error
    identity(y => x = y)

  def g3[T](y: T): Unit =
    var x: T = y     // error
    def foo =
      x = y
    ()

