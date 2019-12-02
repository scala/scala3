import scala.quoted._

trait Quoted {
  val foo: Int
}
inline def quote: Quoted = ${ quoteImpl }

def quoteImpl(given qctx: QuoteContext): Expr[Quoted] = '{
  new Quoted {
    val foo = ???
  }
}
