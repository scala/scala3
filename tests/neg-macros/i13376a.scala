import scala.quoted.*
trait C:
  type T
  def foo: T
inline def makro(inline x: C): x.T = ${ impl[x.type]('x) } // error // error
def impl[CC <: C](xp: Expr[CC])(using Quotes): Expr[CC#T] = '{ $xp.foo }
