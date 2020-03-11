package scala.quoted

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
  def unapply[T](expr: Expr[T])(using unlift: Unliftable[T], qxtc: QuoteContext): Option[T] =
    unlift(expr)

}
