import scala.quoted.{_, given}

object Macro {
  inline def foo: Unit = ${ fooImpl }
  def fooImpl(given QuoteContext): Expr[Unit] = '{}
}
