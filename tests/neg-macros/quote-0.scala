import scala.quoted._

def test(using QuoteContext) = {

  val x: Int = 0

  '{
    val qctx: QuoteContext = ???
    given qctx.type = qctx

    '{x + 1}  // error: wrong staging level

    '{(y: Expr[Int]) => $y }  // error: wrong staging level

  }

  '{x + 1}  // error: wrong staging level

  '{(y: Expr[Int]) => $y }  // error: wrong staging level

}
