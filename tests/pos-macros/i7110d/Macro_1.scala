import scala.quoted._

object Macros {

  inline def m(sym: Symantics) : Int = ${  mImpl('sym) }

  def mImpl(using s: Scope)(sym: s.Expr[Symantics]): s.Expr[Int] = '{
    $sym.Meth(42)
  }
}

trait Symantics  {
  def Meth(exp: Int): Int
}
