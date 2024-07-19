import language.experimental.captureChecking
import compiletime.uninitialized

class File:
  def write(x: String): Unit = ???

class Service:
  var file: File^ = uninitialized  // OK, was error under sealed
  def log = file.write("log") // error, was OK under sealed

def withFile[T](op: (l: caps.Capability) ?-> (f: File^{l}) => T): T =
  op(using caps.cap)(new File)

def test =
  withFile: f =>
    val o = Service()
    o.file = f
    o.log
