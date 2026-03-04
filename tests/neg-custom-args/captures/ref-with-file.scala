import caps.*

class Ref[T](init: T) extends Stateful, Unscoped:
  var x = init
  def get: T = x
  update def put(y: T): Unit = x = y

class File:
  def read(): String = ???

def withFile[T](op: (f: File^) => T): T =
  op(new File)

def withFileAndRef[T](op: (f: File^, r: Ref[String]^) => T): T =
  op(File(), Ref(""))

def Test =
  withFile: f => // error
    val r = Ref(f)
    r
  withFile: f => // was error now ok
    val r = Ref(f)
    ???

  withFileAndRef: (f, r: Ref[String]^) => // error
    r.put(f.read())
    r

