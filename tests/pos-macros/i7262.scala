import scala.quoted._
class Foo {
  def f[T](t: Type[T])(using QuoteContext) = t match {
    case '[ Int *: EmptyTuple ] =>
  }
}
