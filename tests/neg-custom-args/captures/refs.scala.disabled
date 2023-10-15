import java.io.*

type Proc = () => Unit

class Ref[T](init: T):
  var x: T = init
  def setX(x: T): Unit = this.x = x

class MonoRef(init: Proc):
  type MonoProc = Proc
  var x: MonoProc = init
  def setX(x: MonoProc): Unit = this.x = x

def usingLogFile[T](op: (local: caps.Cap) ?-> FileOutputStream^{local} => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

def test1 =
  usingLogFile[Proc]: (local: caps.Cap) ?=>
    (f: FileOutputStream^{local}) =>
      () => f.write(1)  // error (but with a hard to parse error message)
                        // this line has type () ->{local} Unit, but usingLogFile
                        // requires Proc, which expands to () -> 'cap[..test1](from instantiating usingLogFile)

def test2 =
  val r = new Ref[Proc](() => ())
  usingLogFile: f =>
    r.setX(() => f.write(10))  // error
  r.x() // crash: f is closed at that point
  val mr = new MonoRef(() => ())
  usingLogFile[Unit]: f =>
    mr.setX(() => f.write(10))  // error

def test3 =
  val r = new Ref[Proc](() => ())
  usingLogFile[Unit]: f =>
    r.x = () => f.write(10)   // error
  r.x() // crash: f is closed at that point
  val mr = MonoRef(() => ())
  usingLogFile: f =>
    mr.x = () => f.write(10)   // error

def test4 =
  var r: Proc = () => ()
  usingLogFile[Unit]: f =>
    r = () => f.write(10)  // error
  r() // crash: f is closed at that point






