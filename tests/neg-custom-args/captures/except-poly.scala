import caps.{Control, Classifier, SharedCapability}

trait IOCap extends SharedCapability, Classifier
class Console extends IOCap:
  def print(): Unit = ()
class Label extends Control:
  def break(): Unit = ()

class Box[+T]
def run[T, C^](body: () ->{C} T): Box[T]^{C.except[Control]} = ???

def test(console: Console^, l: Label^) =
  val r = run: () =>
    console.print()
    l.break()
  val keepControl: Box[Unit]^{l} = r // error
