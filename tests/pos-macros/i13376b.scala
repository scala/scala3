import scala.quoted.*
trait C:
  type T
  def foo: T
inline def makro(inline x: C): C#T = ${ impl('x) }
def impl[U: Type](xp: Expr[C { def foo: U }])(using Quotes): Expr[U] =
  '{ $xp.foo }
