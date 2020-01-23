import scala.quoted._

def test with QuoteContext = {

  val a = '{1}
  '{
    given QuoteContext = ???
    a // error
    $a
    '{$a} // error
    '{ given QuoteContext = ???; '{$a} } // error
  }

}
