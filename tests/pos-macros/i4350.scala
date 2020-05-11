import scala.quoted._

class Foo[T](using s: Scope)(using s.Type[T]) {
  '{null.asInstanceOf[T]}
}
