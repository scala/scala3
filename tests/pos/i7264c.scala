import scala.quoted._
class Foo {
  def f[T2: Type](e: Expr[T2])(given QuoteContext) = e match {
    case '{ $x: $t0 } =>
      t0 match
        case '[ *:[Int, $t] ] =>
          '[ *:[Int, $t] ]
  }
}
