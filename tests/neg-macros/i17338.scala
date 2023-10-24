import scala.quoted.*

def test(using quotes: Quotes): Expr[Expr[Int]] =
  '{ '{ 1 } } // error
