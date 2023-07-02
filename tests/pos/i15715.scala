import scala.quoted.*

def macroImpl(using Quotes) =
  val expr = Expr(1)
  Some((1, 2)).map { (x, y) =>
    '{ ${expr} + 1 }
  }
