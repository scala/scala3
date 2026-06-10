import scala.quoted.*

class annotWithQuote(e: Expr[Int]) extends scala.annotation.Annotation

def test(using Quotes): Unit =
  @annotWithQuote('{ 1 }) // error
  val x = 1
  ()
