import scala.quoted.{_, given}

trait IsExpr[T] {
  type Underlying
  def expr: Expr[Underlying]
}

def f(x: Any): String = x.toString

def g[T](given e: IsExpr[T], tu: Type[e.Underlying]): (given QuoteContext) => Expr[String] = {
  val underlying: Expr[e.Underlying] = e.expr
  '{f($underlying)}
}
