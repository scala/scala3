import scala.quoted.*

def test(using Quotes)(x: Expr[_]) = {
  import quotes.reflect.*
  x.asTerm.asExprOf[Any]
}

