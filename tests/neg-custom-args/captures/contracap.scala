import language.experimental.captureChecking
import caps.*

class Ref[T](init: T) extends Mutable:
  private var value: T = init
  def get: T = value
  mut def set(newValue: T): Unit = value = newValue

// a library function that assumes that a and b MUST BE separate
def swap[T](a: Ref[Int]^, b: Ref[Int]^): Unit = ???

def test2(): Unit =
  val a: Ref[Int]^ = Ref(0)
  val f: (Ref[Int]^, Ref[Int]^) -> Unit = swap
  val g: (Ref[Int]^{a}, Ref[Int]^{a}) -> Unit = f // error
  g(a, a)  // OH NO