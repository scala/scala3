
import language.experimental.captureChecking
import language.experimental.modularity
import caps.*

case class A()

trait HasCap:
  def mkA: A^{this}

object HasCap:
  def apply[T](body: HasCap^ ?=> T): T = ???

class Box(using tracked val h: HasCap^) extends caps.Stateful:
  var t: A^{h} = h.mkA

def main() =
  HasCap: h1 ?=>
    val b = Box(using h1)
    b.t = h1.mkA
