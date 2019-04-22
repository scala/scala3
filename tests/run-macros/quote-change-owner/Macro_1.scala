import scala.quoted._
object Macros {
  inline def assert2(expr: => Boolean): Unit =  ${ assertImpl('expr) }
  def assertImpl(expr: Expr[Boolean]) = '{
    def foo(): Unit = $expr
    foo()
  }
}
