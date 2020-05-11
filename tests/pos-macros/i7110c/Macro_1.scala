import scala.quoted._

object Macros {

  inline def m[R](sym: Symantics[R]) : R = ${  mImpl[R]('{sym}) }

  def mImpl[R](using s: Scope)(sym: s.Expr[Symantics[R]])(using s.Type[R]): s.Expr[R] =  '{
    $sym.Meth(42)
  }
}

trait Symantics[R]  {
  def Meth(exp: Int): R
}
