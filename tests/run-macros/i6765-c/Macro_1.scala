import scala.quoted._

inline def foo(inline n: Int) = ${fooImpl('n)}

def fooImpl(using s: Scope)(n: s.Expr[Int]) = {
  val res = Expr.ofList(List.tabulate(n.unliftOrError)(i => Expr("#" + i)))
  '{ ${Expr(res.show)} + "\n" + $res.toString + "\n" }
}
