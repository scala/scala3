import scala.quoted._

def test(using Quotes) = {

  '{
    val qctx: Quotes = ???
    given x1 as qctx.type = qctx

    val b = '{3}

    '{
      val qctx: Quotes = ???
      given x2 as qctx.type = qctx

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
