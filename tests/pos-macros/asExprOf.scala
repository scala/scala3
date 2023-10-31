import scala.quoted.*

def test(using Quotes)(x: Expr[?]) = {
  import quotes.reflect.*
  x.asTerm.asExprOf[Any]
}

