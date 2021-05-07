import scala.quoted.*
class Foo {
  def f[T2: Type](e: Expr[T2])(using Quotes) = e match {
    case '{ $x: ${'[List[$t]]} } => // error
    case '{ $x: ${y @ '[List[$t]]} } => // error
  }
}
