import scala.quoted._

trait IsExpr[T] {
  type Underlying
  def expr: Expr[Underlying]
}

def f(x: Any): String = x.toString

def g[T] with (e: IsExpr[T], tu: Type[e.Underlying]) : QuoteContext ?=> Expr[String] = {
  val underlying: Expr[e.Underlying] = e.expr
  '{f($underlying)}
}
