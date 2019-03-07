package scala.tasty.util

import scala.quoted.Expr
import scala.tasty.Reflection

import scala.deprecated

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
@deprecated("Use scala.quoted.matching.Literal", "0.14.0")
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
