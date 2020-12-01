sealed trait Expr[+T]
final case class FooExpr() extends Expr[1 | 2]

object Test {
  def foo[T](x: Expr[T]): T = x match {
    case x: FooExpr =>
      3 // error
  }

  val x: 1 | 2 = foo(FooExpr())
}
