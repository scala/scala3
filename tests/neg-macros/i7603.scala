import scala.quoted._
class Foo {
  def f[T2](using s: Scope)(e: s.Expr[T2])(using s.Type[T2]) = e match {
    case '{ $x: ${'[List[$t]]} } => // error
    case '{ $x: ${y @ '[List[$t]]} } => // error // error
  }
}
