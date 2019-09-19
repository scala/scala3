import scala.quoted.{_, given}
import scala.quoted.given

inline def foo = ${fooImpl}

def fooImpl(given qctx: QuoteContext) = {
  val res = List('{"One"}).toExprOfList
  res.show.toExpr
}
