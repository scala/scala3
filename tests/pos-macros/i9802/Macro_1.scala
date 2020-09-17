import scala.quoted._

inline def fun(inline prog: Double): Double = ${impl('prog)}

def impl(prog: => Expr[Double])(using QuoteContext) : Expr[Double] = '{ 42.0 }
