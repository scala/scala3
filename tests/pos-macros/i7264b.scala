import scala.quoted._
class Foo {
  def f[T2: Type](e: Expr[T2])(using QuoteContext) = e match {
    case '{ $x: *:[Int, t] } =>
      Type.of[ *:[Int, t] ]
  }
}
