import java.io.*

class Ref[T](init: T):
  var x: T = init
  def setX(x: T): Unit = this.x = x

def usingLogFile[sealed T](op: FileOutputStream^ => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result

type Proc = () => Unit
def test1 =
  usingLogFile[Proc]: f => // error
    () =>
      f.write(1)
      ()

def test2 =
  val r = new Ref[Proc](() => ())
   usingLogFile[Unit]: f =>
    r.setX(() => f.write(10))  // should be error
  r.x() // crash: f is closed at that point

def test3 =
  val r = new Ref[Proc](() => ())
   usingLogFile[Unit]: f =>
    r.x = () => f.write(10)   // should be error
  r.x() // crash: f is closed at that point

def test4 =
  var r: Proc = () => ()  // error
   usingLogFile[Unit]: f =>
    r = () => f.write(10)
  r() // crash: f is closed at that point






