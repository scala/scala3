import scala.quoted.{_, given}

def test(given QuoteContext) = {

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
