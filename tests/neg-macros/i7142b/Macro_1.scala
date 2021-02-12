package macros
import scala.quoted.*
import scala.util.control.NonLocalReturns.*

def oops(using Quotes): Expr[Int] =
  returning('{ { (x: Int) => ${ throwReturn('x) }} apply 0 })

inline def test = ${oops}
