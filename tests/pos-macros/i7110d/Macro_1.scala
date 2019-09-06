import scala.quoted._

object Macros {

  inline def m(sym: Symantics) : Int = ${  mImpl('sym) }

  def mImpl(sym: Expr[Symantics]) given (qctx: QuoteContext): Expr[Int] = '{
    $sym.Meth(42)
  }
}

trait Symantics  {
  def Meth(exp: Int): Int
}
