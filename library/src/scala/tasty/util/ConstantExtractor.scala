package scala.tasty.util

import scala.quoted.Expr
import scala.tasty.Reflection

/**
 *  Usage:
 *
 *  ```
 *  val Constant = new ConstantExtractor(reflect)
 *
 *  (x: Expr[B]) match {
 *    case Constant(value) => ...
 *    case x => ...
 *  }
 *  ```
 */
class ConstantExtractor[R <: Reflection with Singleton](val reflect: Reflection) {
  import reflect._

  def unapply[T](expr: Expr[T]): Option[T] = {
    def const(tree: Term): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => const(e)
      case Inlined(_, Nil, e) => const(e)
      case _  => None
    }
    const(expr.unseal)
  }
}
