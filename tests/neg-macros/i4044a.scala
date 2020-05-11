import scala.quoted._

def test(using s: Scope) = {

  def a(using s0: Scope) = '{1}
  '{
    val s: Scope = ???
    given s.type = s
    a // error
    $a
    '{$a} // error
    '{
      val s: Scope = ???
      given s.type = s
      '{$a} // error
    }
  }

}
