import scala.quoted._

class Test {

  def f[T](using s: Scope)(t: s.Type[T], x: s.Expr[T]) = '{
    val z2 = $x // error // error: wrong staging level
  }

  def g[T](using s: Scope)(implicit t: s.Type[T], x: s.Expr[T]) = '{
    val z2 = $x   // ok
  }

}
