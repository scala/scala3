import scala.quoted._

inline def foo = ${fooImpl}

def fooImpl(using Quotes) = {
  import qctx.reflect._
  val res = Expr.ofList(List('{"One"}))
  Expr(res.show)
}
