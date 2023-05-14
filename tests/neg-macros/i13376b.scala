import scala.quoted.*
trait C:
  type T
  def foo: T
inline def makro(x: C): x.T = ${ impl[x.type]('x) }
def impl[CC <: C](xp: Expr[CC])(using Quotes): Expr[CC#T] = '{ $xp.foo } // error
