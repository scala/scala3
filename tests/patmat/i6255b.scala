class Foo {
  def foo(x: quoted.Expr[Int])(using scala.quoted.QuoteContext): Unit = x match {
    case '{ 1 } =>
    case '{ 2 } =>
  }
}
