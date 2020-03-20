import scala.quoted._

object Macros {

  inline def m(sym: Symantics, x: Int) : Int = ${  mImpl('sym, 'x) }

  def mImpl(using qctx: QuoteContext)(sym: Expr[Symantics], x: Expr[Int]): Expr[Int] = '{
    $sym.Meth($x)
  }
}

trait Symantics  {
  def Meth[R](exp: R): Int
  def Meth(): Int
}
