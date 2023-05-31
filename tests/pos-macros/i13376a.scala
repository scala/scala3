import scala.quoted.*
trait C:
  type T
  def foo: T
inline def makro(x: C): x.T = ${ impl[x.T]('x) }
def impl[U: Type](xp: Expr[C { def foo: U }])(using Quotes): Expr[U] =
  '{ $xp.foo }
