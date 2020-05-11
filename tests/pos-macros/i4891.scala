import scala.quoted._

object Test {
  def foo(using s: Scope): s.Expr[Option[String]] = '{None}
}
