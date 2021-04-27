import scala.quoted.*

object Macros {

  inline def m[T](sym: Symantics {type R = T}) : T = ${  mImpl[T]('{sym}) }

  def mImpl[T: Type](using Quotes)(sym: Expr[Symantics { type R = T }]): Expr[T] =  '{
    $sym.Meth(42)
  }
}

trait Symantics {
  type R
  def Meth(exp: Int): R
  def Meth(): R
}
