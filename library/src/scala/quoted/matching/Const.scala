package scala.quoted.matching

import scala.quoted.Expr

import scala.tasty.Reflection // TODO do not depend on reflection directly

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

  def unapply[T](expr: Expr[T])(implicit reflect: Reflection): Option[T] = {
    import reflect._
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.unseal)
  }

}
