package scala.quoted
package util

trait ConstructExpr[T] {

  type Elems <: Tuple

  def from(x: Elems)(using Quotes): Expr[T]
  // def from(x: Tuple.Map[Elems, Expr])(using Quotes): Expr[T] // alternative

}

object ConstructExpr {

  def apply[T](using ce: ConstructExpr[T]): ce.type = ce

    /** Default implementation of `ConstructExpr[Tuple1[T]]` */
  given EmptyTupleConstructExpr[T: Type]: ConstructExpr[EmptyTuple] with {
    type Elems = EmptyTuple
    def from(x: Elems)(using Quotes): Expr[EmptyTuple] =
      '{ EmptyTuple }
  }

  /** Default implementation of `ConstructExpr[Tuple1[T]]` */
  given Tuple1ConstructExpr[T: Type]: ConstructExpr[Tuple1[T]] with {
    type Elems = Tuple1[Expr[T]]
    def from(x: Elems)(using Quotes): Expr[Tuple1[T]] =
      '{ Tuple1[T](${x._1}) }
  }

  /** Default implementation of `ConstructExpr[Tuple2[T1, T2]]` */
  given Tuple2ConstructExpr[T1: Type, T2: Type]: ConstructExpr[Tuple2[T1, T2]] with {
    type Elems = (Expr[T1], Expr[T2])
    def from(x: Elems)(using Quotes): Expr[Tuple2[T1, T2]] =
      '{ Tuple2[T1, T2](${x._1}, ${x._2}) }
  }

  /** Default implementation of `ConstructExpr[Tuple3[T1, T2, T3]]` */
  given Tuple3ConstructExpr[T1: Type, T2: Type, T3: Type]: ConstructExpr[Tuple3[T1, T2, T3]] with {
    type Elems = (Expr[T1], Expr[T2], Expr[T3])
    def from(x: Elems)(using Quotes): Expr[Tuple3[T1, T2, T3]] =
      '{ Tuple3[T1, T2, T3](${x._1}, ${x._2}, ${x._3}) }
  }

}
