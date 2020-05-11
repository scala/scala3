import scala.quoted._

trait IsExpr[T] {
  val s: Scope
  type Underlying
  def expr: s.Expr[Underlying]
}

def f(x: Any): String = x.toString

def g[T](using s0: Scope)(using e: IsExpr[T] { val s: s0.type }, tu: s0.Type[e.Underlying]): s0.Expr[String] = {
  val underlying: s0.Expr[e.Underlying] = e.expr
  '{f($underlying)}
}
