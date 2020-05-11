import scala.quoted._
class Foo {
  def f[T2](using s: Scope)(e: s.Expr[T2])(using s.Type[T2]): Any = e match {
    case '{ $x: *:[Int, $t] } =>
      '[ *:[Int, $t] ]
  }
}
