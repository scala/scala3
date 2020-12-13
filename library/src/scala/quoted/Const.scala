package scala.quoted

/** Literal constant values */
@deprecated("Use `scala.quoted.Expr` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
object Const {

  /** Matches expressions containing literal constant values and extracts the value.
   *
   *  - Converts expression containg literal values to their values:
   *    - `'{1}` -> `1`, `'{2}` -> `2`, ...
   *    - For all primitive types and `String`
   *
   *  Usage:
   *  ```
   *  case '{ ... ${expr @ Const(value)}: T ...} =>
   *    // expr: Expr[T]
   *    // value: T
   *  ```
   *
   *  To directly unlift an expression `expr: Expr[T]` consider using `expr.value`/`expr.valueOrError` insead.
   */
  @deprecated("Use `scala.quoted.Expr.unapply` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
  def unapply[T](expr: Expr[T])(using Quotes): Option[T] = {
    import quotes.reflect._
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) =>
        c match
          case NullConstant() | UnitConstant() | ClassOfConstant(_) => None
          case _ => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Typed(e, _) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.asTerm)
  }

}
