import scala.quoted._

def test given QuoteContext = {

  '{
    given as QuoteContext = ???

    val b = '{3}

    '{
      given as QuoteContext = ???

      b // error
      ${b}
      ${ '{b} } // error
      '{ given as QuoteContext = ???; '{$b} } // error
    }

  }

}
