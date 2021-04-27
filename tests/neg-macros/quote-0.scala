import scala.quoted.*

def test(using Quotes) = {

  val x: Int = 0

  '{
    val qctx: Quotes = ???
    given qctx.type = qctx

    '{x + 1}  // error: wrong staging level

    '{(y: Expr[Int]) => $y }  // error: wrong staging level

  }

  '{x + 1}  // error: wrong staging level

  '{(y: Expr[Int]) => $y }  // error: wrong staging level

}
