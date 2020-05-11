import scala.quoted._

def f() = ()

def triggerStackOverflow(using s: Scope)(n: Int): s.Expr[Double] = {
  val r = triggerStackOverflow(n - 1)
  f()
  r
}

inline def loop(inline prog: Double): Double = ${impl('prog)}

def impl(using s: Scope)(prog: s.Expr[Double]): s.Expr[Double] =
  try {
    triggerStackOverflow(0)
  } catch {
    case e =>
      qctx.tasty.error(e.getMessage, prog.unseal.pos)
      '{ 42.0 }
  }
