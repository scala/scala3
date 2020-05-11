import scala.quoted._

inline def fun(inline prog: Double): Double = ${impl('prog)}

def impl(using s: Scope)(prog: => s.Expr[Double]): s.Expr[Double] = '{ 42.0 }
