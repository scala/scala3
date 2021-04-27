import scala.quoted.*

object Macros {

  inline def m[R](sym: Symantics[R]) : R = ${  mImpl[R]('{sym}) }

  def mImpl[R: Type](using Quotes)(sym: Expr[Symantics[R]]): Expr[R] =  '{
    $sym.Meth(42)
  }
}

trait Symantics[R]  {
  def Meth(exp: Int): R
  def Meth(): R
}
