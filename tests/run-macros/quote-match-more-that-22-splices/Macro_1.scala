import scala.quoted.*

inline def f(inline x: Any) = ${ fExpr('x) }

def fExpr(x: Expr[Any])(using Quotes): Expr[Any] =
  x match
    case '{ ($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21, $x22, $x23, $x24) } =>
      '{ ($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21, $x22, $x23, $x24) }
