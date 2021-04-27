import scala.quoted.*
class Foo {
  def f[T](t: Type[T])(using Quotes) = t match {
    case '[ Int *: EmptyTuple ] =>
  }
}
