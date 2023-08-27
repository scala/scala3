import java.io.*

class Ref[T](init: T):
  var x: T = init
  def setX(x: T): Unit = this.x = x

def usingLogFile[T](op: (local: caps.Root) ?-> FileOutputStream^{local} => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

type Proc = () => Unit
def test1 =
  usingLogFile[Proc]: (local: caps.Root) ?=> // error (but with a hard to parse error message)
    (f: FileOutputStream^{local}) =>
      () => f.write(1)  // this line has type () ->{local} Unit, but usingLogFile
                        // requires Proc, which expands to () -> 'cap[..test1](from instantiating usingLogFile)

def test2 =
  val r = new Ref[Proc](() => ())
   usingLogFile[Unit]: f =>
    r.setX(() => f.write(10))  // error
  r.x() // crash: f is closed at that point

def test3 =
  val r = new Ref[Proc](() => ())
   usingLogFile[Unit]: f =>
    r.x = () => f.write(10)   // error
  r.x() // crash: f is closed at that point

def test4 =
  var r: Proc = () => ()
   usingLogFile[Unit]: f =>
    r = () => f.write(10)  // error
  r() // crash: f is closed at that point






