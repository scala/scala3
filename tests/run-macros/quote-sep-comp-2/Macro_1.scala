import scala.quoted._

object Macros {
  def assertImpl(using s: Scope)(expr: s.Expr[Boolean]) = '{ println($expr) }
}
