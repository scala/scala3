// Compile with -strict -Xfatal-warnings -deprecation
import scala.annotation.infix
class C {
  @infix def op(x: Int): Int = ???
  def meth(x: Int): Int = ???
}

val c = C()
def test() = {
  c op 2
  c.meth(2)

  c.op(2)
  c meth 2    // error: should not be used as infix operator
}
