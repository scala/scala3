package scala.quoted

/** Literal constant values */
object Const {

  /** Matches expressions containing literal constant values and extracts the value.
   *  It may match expressions of type Boolean, Byte, Short, Int, Long,
   *  Float, Double, Char, String, ClassTag, scala.Symbol, Null and Unit.
   *
   *  Usage:
   *  ```
   *  (x: Expr[B]) match {
   *    case Const(value: B) => ...
   *  }
   *  ```
   */
  def unapply[T](expr: Expr[T])(using Quotes): Option[T] = {
    import qctx.reflect._
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Typed(e, _) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(Term.of(expr))
  }

}
