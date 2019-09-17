import scala.quoted._
import scala.quoted.given

inline def foo(inline n: Int) = ${fooImpl(n)}

def fooImpl(n: Int)(given qctx: QuoteContext) = {
  val res = List.tabulate(n)(i => ("#" + i).toExpr).toExprOfList
  '{ ${res.show.toExpr} + "\n" + $res.toString + "\n" }
}
