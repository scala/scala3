package scala.quoted
package matching

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
object Const {

  def unapply[T](expr: Expr[T]) given (qctx: QuoteContext): Option[T] = {
    import qctx.tasty._
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.unseal)
  }

}
