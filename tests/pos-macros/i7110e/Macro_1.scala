import scala.quoted._

object Macros {

  inline def m(sym: Symantics, x: Int) : Int = ${  mImpl('sym, 'x) }

  def mImpl(using s: Scope)(sym: s.Expr[Symantics], x: s.Expr[Int]): s.Expr[Int] = '{
    $sym.Meth($x)
  }
}

trait Symantics  {
  def Meth[R](exp: R): Int
  def Meth(): Int
}
