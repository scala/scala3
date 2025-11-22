import caps.*

class Ref[T](init: T) extends caps.Stateful, Unscoped:
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
/*
  withFileAndRef: (f, r: Ref[String]^) =>
    r.put(f.read())
*/

  withFileAndRef: (f, r) =>
    r.put(f.read())



