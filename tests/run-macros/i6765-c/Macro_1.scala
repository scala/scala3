import scala.quoted._

inline def foo(inline n: Int) = ${fooImpl('n)}

def fooImpl(n: Expr[Int])(using Quotes) = {
  val res = Expr.ofList(List.tabulate(n.unliftOrError)(i => Expr("#" + i)))
  '{ ${Expr(res.show)} + "\n" + $res.toString + "\n" }
}
