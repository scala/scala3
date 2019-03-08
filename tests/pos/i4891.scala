import scala.quoted._

object Test {
  def foo: Expr[Option[String]] = '{None}
}
