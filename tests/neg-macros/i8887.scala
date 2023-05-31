import scala.quoted._

def expr[X](x: Any)(using Quotes): Expr[Any] =
  '{ foo[x.type] } // error
def foo[X]: Any = ???
