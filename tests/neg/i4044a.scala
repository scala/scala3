import scala.quoted._

class Test {

  val a = '{1}
  '{
    a // error
    $a
    '{$a} // error
    '{ '{$a} } // error
  }

}
