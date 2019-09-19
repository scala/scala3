import scala.quoted.{_, given}

object Test {
  def foo(given QuoteContext): Expr[Option[String]] = '{None}
}
