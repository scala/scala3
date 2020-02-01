import scala.quoted._

object Macros {
  def assertImpl(expr: Expr[Boolean])(using QuoteContext) = '{ println($expr) }
}
