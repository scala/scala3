import scala.quoted._

def test(using Quotes)(x: Expr[_]) = {
  import quotes.reflect._
  x.asTerm.asExprOf[Any]
}

