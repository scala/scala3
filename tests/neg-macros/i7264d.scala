import scala.quoted._
class Foo {
  def f[T2: Staged](e: Expr[T2])(using QuoteContext) = e match {
    case '{ $x: *:[Int, Any] } => // error: Staged argument Any does not conform to upper bound Tuple

  }
}
