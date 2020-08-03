import scala.quoted._
object Macro {
  inline def foo[X](x: X): Unit = ${fooImpl('x)}
  def fooImpl[X: quoted.Staged](x: Expr[X])(using QuoteContext): Expr[Unit] = '{}
}
