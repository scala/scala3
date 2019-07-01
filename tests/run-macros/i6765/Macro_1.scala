import scala.quoted._
import delegate scala.quoted._

delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

inline def foo = ${fooImpl}

def fooImpl given (qctx: QuoteContext) = {
  import qctx.tasty._
  val res = List('{"One"}).toExprOfList
  res.show.toExpr
}
