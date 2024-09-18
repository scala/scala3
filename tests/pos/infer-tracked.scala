import scala.language.experimental.modularity
import scala.language.future

abstract class C:
  type T
  def foo: T

class F(val x: C):
  val result: x.T = x.foo

class G(override val x: C) extends F(x)

def Test =
  val c = new C:
    type T = Int
    def foo = 42

  val f = new F(c)
  val i: Int = f.result

  // val g = new G(c)
  // val j: Int = g.result
