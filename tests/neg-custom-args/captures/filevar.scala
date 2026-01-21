import language.experimental.captureChecking
import compiletime.uninitialized

class File:
  def write(x: String): Unit = ???

class Service extends caps.Stateful:
  var file: File^ = uninitialized  //OK, was  error under sealed
  def log = file.write("log") // OK, was error under unsealed

def withFile[T](op: (l: caps.Capability) ?-> (f: File^{l}) => T): T =
  op(using caps.any)(new File)

def test =
  withFile: f =>       // error with level checking, was OK under both schemes before
    val o = Service()
    o.file = f
    o.log
