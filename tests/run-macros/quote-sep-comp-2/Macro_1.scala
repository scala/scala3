import scala.quoted._

object Macros {
  def assertImpl(expr: Expr[Boolean]) = '{ println($expr) }
}
