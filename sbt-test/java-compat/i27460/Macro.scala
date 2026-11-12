import scala.quoted.*

inline def id(inline x: Int): Int = ${ idImpl('x) }
def idImpl(x: Expr[Int])(using Quotes): Expr[Int] = x

def box(x: Int): Any = x
