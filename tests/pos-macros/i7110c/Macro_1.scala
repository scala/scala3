import scala.quoted._

object Macros {

  inline def m[R](sym: Symantics[R]) : R = ${  mImpl[R]('{sym}) }

  def mImpl[R: Type](sym: Expr[Symantics[R]]) given (qctx: QuoteContext): Expr[R] =  '{
    $sym.Meth(42)
  }
}

trait Symantics[R]  {
  def Meth(exp: Int): R
}
