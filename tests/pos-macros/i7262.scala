import scala.quoted._
class Foo {
  def f[T](using s: Scope)(t: s.Type[T]) = t match {
    case '[ Int *: EmptyTuple ] =>
  }
}
