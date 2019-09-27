import scala.quoted.{ QuoteContext, Expr }

def h(m: Expr[M[String]])(given QuoteContext): Expr[Any] = g(m)
