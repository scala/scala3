import scala.quoted.*

inline def foo = ${fooImpl}

def fooImpl(using Quotes) = {
  import quotes.reflect.*
  val res = Expr.ofList(List('{"One"}))
  Expr(res.show)
}
