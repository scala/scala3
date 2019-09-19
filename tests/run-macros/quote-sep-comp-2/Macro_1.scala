import scala.quoted.{_, given}

object Macros {
  def assertImpl(expr: Expr[Boolean])(given QuoteContext) = '{ println($expr) }
}
