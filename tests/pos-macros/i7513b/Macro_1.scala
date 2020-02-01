import scala.quoted._

trait Quoted {
  val foo: Int
}
inline def quote: Quoted = ${ quoteImpl }

def quoteImpl(using qctx: QuoteContext): Expr[Quoted] = '{
  new Quoted {
    val foo = ???
  }
}
