import scala.quoted.{ Quotes, Expr }

def h(m: Expr[M])(using Quotes): Expr[Any] = g(m)