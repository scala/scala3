import scala.quoted.*

inline def foo(inline n: Int) = ${fooImpl('n)}

def fooImpl(n: Expr[Int])(using Quotes) = {
  val res = Expr.ofList(List.tabulate(n.valueOrAbort)(i => Expr("#" + i)))
  '{ ${Expr(res.show)} + "\n" + $res.toString + "\n" }
}
