import scala.quoted.*

trait Quoted {
  val foo: Int
}
inline def quote: Quoted = ${ quoteImpl }

def quoteImpl(using Quotes): Expr[Quoted] = '{
  new Quoted {
    val foo = ???
  }
}
