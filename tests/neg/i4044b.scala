import scala.quoted._

def test(using QuoteContext) = {

  '{
    given QuoteContext = ???

    val b = '{3}

    '{
      given QuoteContext = ???

      b // error
      ${b}
      ${ '{b} } // error
      '{ given QuoteContext = ???; '{$b} } // error
    }

  }

}
