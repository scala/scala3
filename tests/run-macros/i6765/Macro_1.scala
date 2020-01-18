import scala.quoted._
import scala.quoted.given

inline def foo = ${fooImpl}

def fooImpl with (qctx: QuoteContext) = {
  import qctx.tasty.{_, given}
  val res = Expr.ofList(List('{"One"}))
  Expr(res.show)
}
