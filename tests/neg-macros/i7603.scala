import scala.quoted._
class Foo {
  def f[T2: Type](e: Expr[T2])(using QuoteContext) = e match {
    case '{ $x: ${Type[List[$t]]} } => // error
    case '{ $x: ${y @ Type[List[$t]]} } => // error // error
  }
}
