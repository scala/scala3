import caps.{any, Classifier, Control, SharedCapability}

trait IOCap extends SharedCapability, Classifier

class File extends IOCap:
  def read(): Int = 1

class Label extends Control

def runOnNewThread[T](body: () ->{any.except[Control]} T): T = body()

def test(f: File^, l: Label^, o: Object^) =
  runOnNewThread: () =>
    f.read() // ok: f is classified as IOCap, which is unrelated to Control
    println(l) // error: l is classified as Control
  runOnNewThread: () =>
    println(o) // error: o is unclassified, so it could capture Control capabilities

def test2(c: Object^): Unit =
  val x1: Object^{c} = ???
  val y1: Object^{c.except[IOCap]} = x1 // error: {c} is not included in {c.except[IOCap]}
  val x2: Object^{c.except[Control]} = ???
  val y2: Object^{c.except[IOCap]} = x2 // error: unrelated exclusions do not subsume each other
