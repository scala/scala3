import scala.quoted._

object Macros {
  inline def power(inline n: Int) = ${ powerCode('n) }
  def powerCode(using s: Scope)(n: s.Expr[Int]): s.Expr[Double] = ???
}

def run(using s: Scope) = '{
  Macros.power(3)
  ???
}
