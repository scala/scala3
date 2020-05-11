class Foo {
  def foo(using s: quoted.Scope)(x: s.Expr[Int]): Unit = x match {
    case '{ 1 } =>
    case '{ 2 } =>
    case _ =>
  }
}
