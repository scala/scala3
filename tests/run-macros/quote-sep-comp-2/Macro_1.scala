import scala.quoted._

object Macros {
  def assertImpl(expr: Expr[Boolean]) given QuoteContext = '{ println($expr) }
}
