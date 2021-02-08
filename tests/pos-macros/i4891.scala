import scala.quoted.*

object Test {
  def foo(using Quotes): Expr[Option[String]] = '{None}
}
