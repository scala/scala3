import scala.quoted.*
object Macro {
  inline def foo[X](x: X): Unit = ${fooImpl('x)}
  def fooImpl[X: Type](x: Expr[X])(using Quotes): Expr[Unit] = '{}
}
