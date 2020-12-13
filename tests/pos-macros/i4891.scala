import scala.quoted._

object Test {
  def foo(using Quotes): Expr[Option[String]] = '{None}
}
