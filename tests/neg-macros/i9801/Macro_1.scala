import scala.quoted._

def f() = ()

def triggerStackOverflow(n: Int): Expr[Double] = {
  val r = triggerStackOverflow(n - 1)
  f()
  r
}

inline def loop(inline prog: Double): Double = ${impl('prog)}

def impl(prog: Expr[Double])(using QuoteContext) : Expr[Double] =
  try {
    triggerStackOverflow(0)
  } catch {
    case e =>
      qctx.reflect.Reporting.error(e.getMessage, prog.asReflectTree.pos)
      '{ 42.0 }
  }
