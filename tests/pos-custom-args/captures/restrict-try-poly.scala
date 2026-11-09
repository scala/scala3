import caps.{Control, Classifier, SharedCapability}

trait IOCap extends SharedCapability, Classifier
class Console extends IOCap:
  def print(): Unit = ()
class Label extends Control:
  def break(): Unit = ()

class Try[+T]
object Try:
  def apply[T, C^](body: () ->{C} T): Try[T]^{C.only[Control]} = ???

def test(console: Console^, l: Label^) =
  val t = Try: () =>
    console.print()
    l.break()
  val keepControl: Try[Unit]^{l}          = t
  val widen:       Try[Unit]^{l, console} = t
