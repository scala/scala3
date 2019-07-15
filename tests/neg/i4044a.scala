import scala.quoted._

def test given QuoteContext = {

  val a = '{1}
  '{
    given as QuoteContext = ???
    a // error
    $a
    '{$a} // error
    '{ given as QuoteContext = ???; '{$a} } // error
  }

}
