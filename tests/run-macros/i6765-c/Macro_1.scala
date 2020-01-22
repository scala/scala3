import scala.quoted._
import scala.quoted.{given _}

inline def foo(inline n: Int) = ${fooImpl('n)}

def fooImpl(n: Expr[Int])with (qctx: QuoteContext)  = {
  val res = Expr.ofList(List.tabulate(n.value)(i => Expr("#" + i)))
  '{ ${Expr(res.show)} + "\n" + $res.toString + "\n" }
}
