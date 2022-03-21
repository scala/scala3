import quoted.*

inline def isFoo(inline x: Any): Boolean = ${ isFooImpl('x) }

def isFooImpl(x: Expr[Any])(using Quotes): Expr[Boolean] =
  x match
    case '{ ($p: Parent).foo } => '{ true }
    case _ => '{ false }

trait Parent:
  def foo = 0
