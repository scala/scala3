import scala.quoted.{ QuoteContext, Expr }

def h(m: Expr[Foo])(using QuoteContext): Expr[Any] = g(m)
