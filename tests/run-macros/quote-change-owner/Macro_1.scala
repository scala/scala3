import scala.quoted.{_, given}
object Macros {
  inline def assert2(expr: => Boolean): Unit =  ${ assertImpl('expr) }
  def assertImpl(expr: Expr[Boolean])(given QuoteContext) = '{
    def foo(): Unit = $expr
    foo()
  }
}
