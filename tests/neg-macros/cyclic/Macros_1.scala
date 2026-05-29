//> using options -Xmax-inlines:100000

package macros

import scala.quoted.*


transparent inline def createTuple[N <: Int, A](inline value: A): Tuple =
  ${ createTupleImpl[N, A]('value) }

def createTupleImpl[N <: Int: Type, A: Type](value: Expr[A])(using q: Quotes): Expr[Tuple] =
  import q.reflect.*

  transparent inline def createTupleAux(x: Int): Expr[Tuple] =
    if x == 0 then '{ EmptyTuple }
    else '{ $value *: ${ createTupleAux(x - 1) } }

  TypeRepr.of[N] match
    case ConstantType(IntConstant(x)) if x >= 1 => createTupleAux(x) // error
    case _ => report.errorAndAbort(s"invalid tuple size ${Type.show[N]}")
