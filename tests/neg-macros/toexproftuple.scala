import scala.quoted._, scala.deriving.*

inline def mcr: Any = ${mcrImpl}

def mcrImpl(using ctx: Quotes): Expr[Any] = {

  val tpl: (Expr[1], Expr[2], Expr[3]) = ('{1}, '{2}, '{3})
  '{val res: (1, 3, 3) = ${Expr.ofTuple(tpl)}; res}  // error
  //                          ^^^^^^^^^^^^^^^^^
  // Found:    quoted.Expr[(1 : Int) *: (2 : Int) *: (3 : Int) *: EmptyTuple]
  // Required: quoted.Expr[((1 : Int), (3 : Int), (3 : Int))]

  val tpl2: (Expr[1], 2, Expr[3]) = ('{1}, 2, '{3})
  '{val res = ${Expr.ofTuple(tpl2)}; res}  // error
  //                                 ^
  // Cannot prove that (quoted.Expr[(1 : Int)], (2 : Int), quoted.Expr[(3 : Int)]) =:= scala.Tuple.Map[
  //   scala.Tuple.InverseMap[
  //     (quoted.Expr[(1 : Int)], (2 : Int), quoted.Expr[(3 : Int)])
  //   , quoted.Expr]
  // , quoted.Expr].

}
