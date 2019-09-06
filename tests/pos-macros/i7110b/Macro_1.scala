import scala.quoted._

object Macros {

  inline def m[T](sym: Symantics {type R = T}) : T = ${  mImpl[T]('{sym}) }

  def mImpl[T: Type](sym: Expr[Symantics { type R = T }]) given (qctx: QuoteContext): Expr[T] =  '{
    $sym.Meth(42)
  }
}

trait Symantics {
  type R
  def Meth(exp: Int): R
  def Meth(): R
}
