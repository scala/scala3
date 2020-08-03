import scala.quoted._

object Macros {

  inline def m[R](sym: Symantics[R]) : R = ${  mImpl[R]('{sym}) }

  def mImpl[R: Staged](using qctx: QuoteContext)(sym: Expr[Symantics[R]]): Expr[R] =  '{
    $sym.Meth(42)
  }
}

trait Symantics[R]  {
  def Meth(exp: Int): R
  def Meth(): R
}
