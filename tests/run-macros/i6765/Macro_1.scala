import scala.quoted._
import given scala.quoted._

inline def foo = ${fooImpl}

def fooImpl given (qctx: QuoteContext) = {
  import qctx.tasty._
  val res = List('{"One"}).toExprOfList
  res.show.toExpr
}
