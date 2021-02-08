import scala.quoted.*

class Test {

  def f[T](t: Type[T], x: Expr[T])(using Quotes) = '{
    val z2 = $x // error // error: wrong staging level
  }

  def g[T](implicit t: Type[T], x: Expr[T], qctx: Quotes) = '{
    val z2 = $x   // ok
  }

}
