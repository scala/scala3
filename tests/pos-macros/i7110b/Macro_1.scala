import scala.quoted._

object Macros {

  inline def m[T](sym: Symantics {type R = T}) : T = ${  mImpl[T]('{sym}) }

  def mImpl[T](using s: Scope)(sym: s.Expr[Symantics { type R = T }])(using s.Type[T]): s.Expr[T] =  '{
    $sym.Meth(42)
  }
}

trait Symantics {
  type R
  def Meth(exp: Int): R
  def Meth(): R
}
