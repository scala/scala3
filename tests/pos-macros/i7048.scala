import scala.quoted.*

trait IsExpr[T] {
  type Underlying
  def expr: Expr[Underlying]
}

def f(x: Any): String = x.toString

def g[T](using e: IsExpr[T], tu: Type[e.Underlying]): Quotes ?=> Expr[String] = {
  val underlying: Expr[e.Underlying] = e.expr
  '{f($underlying)}
}
