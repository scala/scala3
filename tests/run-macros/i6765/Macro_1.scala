import scala.quoted._
import given scala.quoted._

given as Toolbox = Toolbox.make(getClass.getClassLoader)

inline def foo = ${fooImpl}

def fooImpl given (qctx: QuoteContext) = {
  import qctx.tasty._
  val res = List('{"One"}).toExprOfList
  res.show.toExpr
}
