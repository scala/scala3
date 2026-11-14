package p
import caps.*

class File extends SharedCapability:
  def read(): Unit = ()

class Holder(val c: File):
  // The inferred types of `f` and `g` keep the capture sets of their parameter types
  val f = (x: File^{c}) => x.read()
  val g = [C^] => (x: File^{C}) => x.read()
