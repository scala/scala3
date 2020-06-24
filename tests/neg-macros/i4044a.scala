import scala.quoted._

def test(using QuoteContext) = {

  val a = '{1}
  '{
    val qctx: QuoteContext = ???
    given qctx.type = qctx
    a // error
    $a
    '{$a} // error
    '{
      val qctx: QuoteContext = ???
      given qctx.type = qctx
      '{$a} // error
    }
  }

}
