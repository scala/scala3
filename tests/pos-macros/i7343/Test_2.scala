import scala.quoted.{ QuoteContext, Expr }

def h(m: Expr[M])(using QuoteContext): Expr[Any] = g(m)