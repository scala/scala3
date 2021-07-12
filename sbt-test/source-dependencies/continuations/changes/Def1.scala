import scala.util.continuations._

class Baz {
  def foo = shiftUnit[Foo with BarA, Unit, Unit](null)
}
