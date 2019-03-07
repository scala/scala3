package scala.quoted.matching

import scala.quoted.Expr

import scala.tasty.Reflection // TODO do not depend on reflection directly

/** Matches expressions containing literal values and extracts the value.
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
    import reflect._
    def literal(tree: Term): Option[T] = tree match {
      case Term.Literal(c) => Some(c.value.asInstanceOf[T])
      case Term.Block(Nil, e) => literal(e)
      case Term.Inlined(_, Nil, e) => literal(e)
      case _  => None
    }
    literal(expr.unseal)
  }

}
