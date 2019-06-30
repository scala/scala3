import scala.quoted._

def test given QuoteContext = {

  '{

    val b = '{3}

    '{
      b // error
      ${b}
      ${ '{b} } // error
      '{ '{$b} } // error
    }

  }

}
