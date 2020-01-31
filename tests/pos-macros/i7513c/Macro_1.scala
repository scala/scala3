import scala.quoted._

object Macros {
  trait Quoted {
    def foo: Int
  }
  inline def quote: Quoted = ${ quoteImpl }

  def quoteImpl(given qctx: QuoteContext): Expr[Quoted] = '{
    new Quoted {
      def foo = ???
    }
  }
}
