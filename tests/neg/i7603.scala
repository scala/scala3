import scala.quoted._
class Foo {
  def f[T2: Type](e: Expr[T2])(given QuoteContext) = e match {
    case '{ $x: ${'[List[$t]]} } => // error
    case '{ $x: ${y @ '[List[$t]]} } => // error // error
  }
}
