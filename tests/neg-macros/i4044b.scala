import scala.quoted._

def test(using Quotes) = {

  '{
    val qctx: Quotes = ???
    given qctx.type as x1 = qctx

    val b = '{3}

    '{
      val qctx: Quotes = ???
      given qctx.type as x2 = qctx

      b // error
      ${b}
      ${ '{b} } // error
      '{
        val qctx: Quotes = ???
        given qctx.type = qctx
        '{$b} // error
      }
    }

  }

}
