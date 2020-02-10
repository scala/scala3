import scala.quoted._

object Macros {

  inline def foo: A = ${ fooImpl }

  def fooImpl(using qctx: QuoteContext): Expr[A] = {
    new B().f
    '{ new A; }
  }
}
