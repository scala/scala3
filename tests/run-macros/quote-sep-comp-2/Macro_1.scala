import scala.quoted.*

object Macros {
  def assertImpl(expr: Expr[Boolean])(using Quotes) = '{ println($expr) }
}
