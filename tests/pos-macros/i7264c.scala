import scala.quoted._
class Foo {
  def f[T2: Type](e: Expr[T2])(using QuoteContext) = e match {
    case '{ $x: $T0 } =>
      Type[T0] match
        case '[ *:[Int, $T] ] =>
          '[ *:[Int, T] ]
  }
}
