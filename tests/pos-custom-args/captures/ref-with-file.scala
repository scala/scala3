import caps.*

class Ref[T](init: T) extends Mutable, Unscoped:
  var x = init
  def get: T = x
  update def put(y: T): Unit = x = y

class File:
  def read(): String = ???

def withFile[T](op: (f: File^) => T): T =
  op(new File)

def Test =
  withFile: f =>
    val r = Ref(f.read())
    r


