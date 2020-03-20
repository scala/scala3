import scala.quoted._

object Macros {

  inline def m(sym: Symantics) : Int = ${  mImpl('sym) }

  def mImpl(using qctx: QuoteContext)(sym: Expr[Symantics]): Expr[Int] = '{
    $sym.Meth(42)
  }
}

trait Symantics  {
  def Meth(exp: Int): Int
}
