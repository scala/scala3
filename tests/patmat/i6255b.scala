// scalac: -Ycheck-all-patmat
class Foo {
  def foo(x: quoted.Expr[Int])(using scala.quoted.Quotes): Unit = x match {
    case '{ 1 } =>
    case '{ 2 } =>
  }
}
