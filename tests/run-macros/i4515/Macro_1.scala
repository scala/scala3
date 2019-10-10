import scala.quoted._
object Macro {
  inline def foo[X](x: X): Unit = ${fooImpl('x)}
  def fooImpl[X: TypeTag](x: Expr[X])(given QuoteContext): Expr[Unit] = '{}
}
