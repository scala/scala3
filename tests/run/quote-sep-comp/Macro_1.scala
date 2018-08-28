import scala.quoted._
object Macros {
  rewrite def assert2(expr: => Boolean): Unit =  ~ assertImpl('(expr))
  def assertImpl(expr: Expr[Boolean]) = '{ println(~expr) }
}
