import scala.quoted._
object Macros {
  inline def assert(expr: => Boolean): Unit =  ~ assertImpl('(expr))
  def assertImpl(expr: Expr[Boolean]) = '{ () }
}
