import scala.quoted._

object Test {
  def foo with QuoteContext : Expr[Option[String]] = '{None}
}
