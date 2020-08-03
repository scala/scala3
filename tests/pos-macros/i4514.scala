import scala.quoted._
object Foo {
  inline def foo[X](x: X): Unit = ${fooImpl('x)}
  def fooImpl[X: Staged](x: X)(using QuoteContext): Expr[Unit] = '{}
}
