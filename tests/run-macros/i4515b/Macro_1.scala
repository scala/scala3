import scala.quoted._

object Macro {
  inline def foo: Unit = ${ fooImpl }
  def fooImpl given QuoteContext: Expr[Unit] = '{}
}
