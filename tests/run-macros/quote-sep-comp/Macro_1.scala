import scala.quoted.*
object Macros {
  inline def assert2(expr: => Boolean): Unit =  ${ assertImpl('expr) }
  def assertImpl(expr: Expr[Boolean])(using Quotes) = '{ println($expr) }
}
