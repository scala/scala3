import scala.quoted.{_, given}

def test(given QuoteContext) = {

  val a = '{1}
  '{
    given QuoteContext = ???
    a // error
    $a
    '{$a} // error
    '{ given QuoteContext = ???; '{$a} } // error
  }

}
