import scala.quoted._
import scala.quoted.given

inline def foo = ${fooImpl}

def fooImpl(given qctx: QuoteContext) = {
  val res = Expr.ofList(List('{"One"}))
  res.show.toExpr
}
