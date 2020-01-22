package scala.quoted
package matching

/** Value expressions */
object Value {

  /** Matches expressions containing values and extracts the value.
   *
   *  Usage:
   *  ```
   *  (x: Expr[B]) match {
   *    case Value(value) => ... // value: B
   *  }
   *  ```
   */
  def unapply[T](expr: Expr[T])(given valueOf: ValueOfExpr[T], qxtc: QuoteContext): Option[T] =
    valueOf(expr)

}
