import scala.quoted._

class Test {

  def f[T](t: TypeTag[T], x: Expr[T])(given QuoteContext) = '{
    val z2 = $x // error // error: wrong staging level
  }

  def g[T](implicit t: TypeTag[T], x: Expr[T], qctx: QuoteContext) = '{
    val z2 = $x   // ok
  }

}
