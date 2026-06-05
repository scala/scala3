import scala.compiletime.requireConst

inline def nonTransp(inline f: Int => Int): Int = f(1)
transparent inline def transp(inline f: Int => Int): Int = f(2)
inline def nonTranspDouble: Int => Int = x => x * 2
transparent inline def transpDouble: Int => Int = x => x * 2

def Test =
  requireConst(nonTransp(nonTranspDouble))
  requireConst(nonTransp(transpDouble))
  requireConst(transp(nonTranspDouble)) // error
  requireConst(transp(transpDouble))
