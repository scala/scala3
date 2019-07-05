import scala.quoted._

object Test {
  def foo given QuoteContext: Expr[Option[String]] = '{None}
}
