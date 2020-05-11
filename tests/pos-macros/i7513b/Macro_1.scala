import scala.quoted._

trait Quoted {
  val foo: Int
}
inline def quote: Quoted = ${ quoteImpl }

def quoteImpl(using s: Scope): s.Expr[Quoted] = '{
  new Quoted {
    val foo = ???
  }
}
