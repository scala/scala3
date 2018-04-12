import scala.util.continuations._

class Baz {
  def foo = shiftUnit[Foo with BarB, Unit, Unit](null)
}
