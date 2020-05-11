import scala.quoted._

def test(using Scope) = {

  '{
    val s: Scope = ???
    given x1 as s.type = s

    val b = '{3}

    '{
      val s: Scope = ???
      given x2 as s.type = s

      b // error
      ${b}
      ${ '{b} } // error
      '{
        val s: Scope = ???
        given s.type = s
        '{$b} // error
      }
    }

  }

}
