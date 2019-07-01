class Foo {
  def foo(x: quoted.Expr[Int]) given scala.quoted.QuoteContext: Unit = x match {
    case '{ 1 } =>
    case '{ 2 } =>
    case _ =>
  }
}
