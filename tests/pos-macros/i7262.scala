import scala.quoted._
class Foo {
  def f[T](t: Staged[T])(using QuoteContext) = t match {
    case '[ Int *: EmptyTuple ] =>
  }
}
