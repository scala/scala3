import scala.quoted._

inline def foo = ${fooImpl}

def fooImpl(using Quotes) = {
  import quotes.reflect._
  val res = Expr.ofList(List('{"One"}))
  Expr(res.show)
}
