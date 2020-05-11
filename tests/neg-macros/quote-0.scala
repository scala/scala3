import scala.quoted._

def test(using s: Scope) = {

  val x: Int = 0

  '{
    val s: Scope = ???
    given s.type = s

    '{x + 1}  // error: wrong staging level

    '{(y: scope.Expr[Int]) => $y }  // error // error: wrong staging level

  }

  '{x + 1}  // error: wrong staging level

  '{(y: scope.Expr[Int]) => $y }  // error // error: wrong staging level

}
