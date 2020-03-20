
trait Symantics[R]  {
  def Meth(exp: Int): R
  def Meth(): R
}

inline def m[R](sym: Symantics[R]) : R =
  sym.Meth(42)

@main def Test: Unit = {

  val sym = new Symantics[Int] {
    def Meth(exp: Int): Int = exp
    def Meth(): Int = 43
  }

  val test = m[Int](sym)
}
