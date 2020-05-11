import scala.quoted._
class Foo {
  def f[T2](using s: Scope)(e: s.Expr[T2])(using s.Type[T2]): Unit = e match {
    case '{ $x: $t0 } =>
      t0 match
        case '[ *:[Int, $t] ] =>
          '[ *:[Int, $t] ]
  }
}
