import scala.quoted._

object Macro {
  inline def foo: Unit = ${ fooImpl }
  def fooImpl with QuoteContext : Expr[Unit] = '{}
}
