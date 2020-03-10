import scala.quoted._

def test(using QuoteContext) = {

  '{
    val qctx: QuoteContext = ???
    given qctx.type = qctx

    val b = '{3}

    '{
      val qctx: QuoteContext = ???
      given qctx.type = qctx

      b // error
      ${b}
      ${ '{b} } // error
      '{
        val qctx: QuoteContext = ???
        given qctx.type = qctx
        '{$b} // error
      }
    }

  }

}
