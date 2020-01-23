import scala.quoted._
import scala.quoted.{given _}

inline def foo = ${fooImpl}

def fooImpl with (qctx: QuoteContext) = {
  val res = Expr.ofList(List('{"One"}))
  Expr(res.show)
}
