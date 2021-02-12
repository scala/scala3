import scala.quoted.*

def test(using Quotes) = {

  val a = '{1}
  '{
    val qctx: Quotes = ???
    given qctx.type = qctx
    a // error
    $a
    '{$a} // error
    '{
      val qctx: Quotes = ???
      given qctx.type = qctx
      '{$a} // error
    }
  }

}
