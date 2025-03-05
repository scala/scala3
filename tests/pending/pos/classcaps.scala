
import language.experimental.captureChecking
import caps.*

case class A()

trait HasCap:
  def mkA: A^{this}

object HasCap:
  def apply[T](body: HasCap^ ?=> T): T = ???

class Box(using h: HasCap^):
  var t: A^{h} = h.mkA

def main() =
  HasCap: h ?=>
    val b = Box(using h)
    b.t = h.mkA