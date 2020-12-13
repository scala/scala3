import scala.quoted._
class Foo {
  def f[T](t: Type[T])(using Quotes) = t match {
    case '[ Int *: EmptyTuple ] =>
  }
}
