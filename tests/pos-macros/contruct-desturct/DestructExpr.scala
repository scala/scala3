package scala.quoted
package util

trait DestructExpr[T] {

  type Elems <: Tuple

  def unapply(x: Expr[T])(using Quotes): Option[Elems]
  // def unapply(x: Expr[T])(using Quotes): Option[Tuple.Map[Elems, Expr]] // alternative

}

/** Default given instances of `DestructExpr` */
object DestructExpr {

  def unapply[T](x: Expr[T])(using de: DestructExpr[T])(using Quotes): Option[de.Elems] =
    de.unapply(x)


  /** Default implementation of `DestructExpr[Tuple1[...]]`
   *  - Transform `'{Tuple1(x1)}` into `Some(Tuple1('{x1}))`
   *  - Otherwise returns `None`
   */
  given DestructTuple1[T1](using Type[T1]): DestructExpr[Tuple1[T1]] with {
    type Elems = Tuple1[Expr[T1]]
    def unapply(x: Expr[Tuple1[T1]])(using Quotes) = x match {
      case '{ new Tuple1[T1]($y) } => Some(Tuple1(y))
      case '{     Tuple1[T1]($y) } => Some(Tuple1(y))
      case _ => None
    }
  }

  /** Default implementation of `DestructExpr[Tuple2[...]]`
   *  - Transform `'{Tuple2(x1, x2)}` into `Some(Tuple2('{x1}, '{x2}))`
   *  - Otherwise returns `None`
   */
  given DestructTuple2[T1, T2](using Type[T1], Type[T2]): DestructExpr[Tuple2[T1, T2]] with {
    type Elems = (Expr[T1], Expr[T2])
    def unapply(x: Expr[Tuple2[T1, T2]])(using Quotes) = x match {
      case '{ new Tuple2[T1, T2]($y1, $y2) } => Some(Tuple2(y1, y2))
      case '{     Tuple2[T1, T2]($y1, $y2) } => Some(Tuple2(y1, y2))
      case '{ ($y1: T1) -> ($y2: T2) } => Some(Tuple2(y1, y2))
      case _ => None
    }
  }

}
