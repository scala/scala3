type Expr[T]
type G[X]

def f(word: String): Expr[G[String]] = word.toExpr // error

def splice[T](e: Expr[T]): T = ???

type Liftable
given Liftable = ???

implicit object ExprOps {
  def (x: T).toExpr[T] with Liftable : Expr[T] = ???
}
