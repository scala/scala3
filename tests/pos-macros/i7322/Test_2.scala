import scala.quoted.{ QuoteContext, Expr }

def h(m: Expr[M[String]])(using QuoteContext): Expr[Any] = g(m)