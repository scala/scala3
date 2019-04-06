package scala.quoted.matching

import scala.quoted.Expr

import scala.tasty.Reflection // TODO do not depend on reflection directly

/** Matches expressions containing literal values and extracts the value.
 *  It may match expressions of type Boolean, Byte, Short, Int, Long,
 *  Float, Double, Char, String, ClassTag, scala.Symbol, Null and Unit.
 *
 *  Usage:
 *  ```
 *  (x: Expr[B]) match {
 *    case Literal(value: B) => ...
 *  }
 *  ```
 */
object Literal {

  def unapply[T](expr: Expr[T])(implicit reflect: Reflection): Option[T] = {
    import reflect.{Literal => LiteralTree, _} // TODO rename reflect.Literal to avoid this clash
    def literal(tree: Term): Option[T] = tree match {
      case LiteralTree(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => literal(e)
      case Inlined(_, Nil, e) => literal(e)
      case _  => None
    }
    literal(expr.unseal)
  }

}
