import scala.util.continuations._

class Baz {
  def foo = shiftUnit[Foo & BarA, Unit, Unit](null)
}
