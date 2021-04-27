import scala.quoted.{ Quotes, Expr }

def h(m: Expr[Foo])(using Quotes): Expr[Any] = g(m)
