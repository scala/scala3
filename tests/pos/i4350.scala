import scala.quoted._

class Foo[T: Type] {
  def foo: Staged[T] = '{null.asInstanceOf[T]}
}
