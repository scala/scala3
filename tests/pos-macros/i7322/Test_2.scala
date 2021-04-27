import scala.quoted.{ Quotes, Expr }

def h(m: Expr[M[String]])(using Quotes): Expr[Any] = g(m)