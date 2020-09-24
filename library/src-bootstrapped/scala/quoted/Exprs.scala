package scala.quoted

import scala.quoted.show.SyntaxHighlight

trait Exprs { self: Scope =>

  private given self.type = self // FIXME remove

  type Expr[+T] <: tasty.Term

  object Expr:

    /** Lift a value into an expression containing the construction of that value */
    def apply[T](x: T)(using lift: Liftable[T]): Expr[T] = lift.toExpr(x)

  end Expr


  /** Pattern matches `this` against `that`. Effectively performing a deep equality check.
   *  It does the equivalent of
   *  ```
   *  this match
   *    case '{...} => true // where the contents of the pattern are the contents of `that`
   *    case _ => false
   *  ```
   */
  extension (expr: Expr[Any])
    def matches(that: Expr[Any]): Boolean =
      !self.tasty.termMatch(expr, that, false).isEmpty

  /** Checked cast to a `quoted.Expr[X]` */
  extension [X](expr: Expr[Any])
    def cast(using tp: Type[X]): Expr[X] = expr.asExprOf[X]


  /** Return the unlifted value of this expression.
   *
   *  Returns `None` if the expression does not contain a value or contains side effects.
   *  Otherwise returns the `Some` of the value.
   */
  extension [T](expr: Expr[T])
    def unlift(using unlift: Unliftable[T]): Option[T] =
      unlift.fromExpr(expr)

  /** Return the unlifted value of this expression.
   *
   *  Emits an error and throws if the expression does not contain a value or contains side effects.
   *  Otherwise returns the value.
   */
  extension [T](expr: Expr[T])
    final def unliftOrError(using unlift: Unliftable[T]): T =
      unlift.fromExpr(expr).getOrElse(report.throwErrorOn(expr, s"Expected a known value. \n\nThe value of: ${expr.show}\ncould not be unlifted using $unlift"))

}
