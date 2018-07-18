import scala.quoted._
object Macros {
  transparent def assert2(expr: => Boolean): Unit =  ~ assertImpl('(expr))
  def assertImpl(expr: Expr[Boolean]) = '{ println(~expr) }
}
