import scala.quoted._
object Macros {
  inline def assert2(expr: => Boolean): Unit =  ${ assertImpl('expr) }
  def assertImpl(using s: Scope)(expr: s.Expr[Boolean]) = '{ println($expr) }
}
