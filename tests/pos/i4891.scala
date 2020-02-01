import scala.quoted._

object Test {
  def foo(using QuoteContext): Expr[Option[String]] = '{None}
}
