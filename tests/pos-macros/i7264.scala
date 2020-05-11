import scala.quoted._
class Foo {
  def f[T2](using s: Scope)(t: s.Type[T2]): Any = t match {
    case '[ *:[Int, $t] ] =>
      '[ *:[Int, $t] ]
  }
}