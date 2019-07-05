import scala.quoted._

def test given QuoteContext = {

  val a = '{1}
  '{
    a // error
    $a
    '{$a} // error
    '{ '{$a} } // error
  }

}
