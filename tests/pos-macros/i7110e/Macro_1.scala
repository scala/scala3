import scala.quoted.*

object Macros {

  inline def m(sym: Symantics, x: Int) : Int = ${  mImpl('sym, 'x) }

  def mImpl(using Quotes)(sym: Expr[Symantics], x: Expr[Int]): Expr[Int] = '{
    $sym.Meth($x)
  }
}

trait Symantics  {
  def Meth[R](exp: R): Int
  def Meth(): Int
}
