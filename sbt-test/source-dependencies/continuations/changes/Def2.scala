import scala.util.continuations._

class Baz {
  def foo = shiftUnit[Foo & BarB, Unit, Unit](null)
}
