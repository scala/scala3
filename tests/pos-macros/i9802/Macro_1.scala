import scala.quoted.*

inline def fun(inline prog: Double): Double = ${impl('prog)}

def impl(prog: => Expr[Double])(using Quotes) : Expr[Double] = '{ 42.0 }
