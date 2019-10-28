import scala.quoted._

object Macros {

  inline def foo: A = ${ fooImpl }

  def fooImpl(given qctx: QuoteContext): Expr[A] = {
    new B().f
    '{ new A; }
  }
}
