type Expr[T]
type G[X]

def f(word: String): Expr[G[String]] = word.toExpr // error

def splice[T](e: Expr[T]): T = ???

type Liftable
given Liftable = ???

implicit object ExprOps {
  extension [T](x: T) def toExpr(using Liftable): Expr[T] = ???
}
