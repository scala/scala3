import scala.quoted.*
class Foo {
  def f[T2: Type](e: Expr[T2])(using Quotes) = e match {
    case '{ $x: *:[Int, Any] } => // error: Type argument Any does not conform to upper bound Tuple

  }
}
