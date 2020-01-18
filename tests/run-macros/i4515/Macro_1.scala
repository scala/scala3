import scala.quoted._
object Macro {
  inline def foo[X](x: X): Unit = ${fooImpl('x)}
  def fooImpl[X: quoted.Type](x: Expr[X]) with QuoteContext : Expr[Unit] = '{}
}
