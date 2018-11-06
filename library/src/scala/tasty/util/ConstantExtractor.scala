package scala.tasty.util

import scala.quoted.Expr
import scala.tasty.Tasty

/**
 *  Usage:
 *
 *  ```
 *  val Constant = new ConstantExtractor(tasty)
 *
 *  (x: Expr[B]) match {
 *    case Constant(value) => ...
 *    case x => ...
 *  }
 *  ```
 */
class ConstantExtractor[T <: Tasty with Singleton](val tasty: T) {
  import tasty._

  def unapply[T](expr: Expr[T]): Option[T] = {
    def const(tree: Term): Option[T] = tree match {
      case Term.Literal(c) => Some(c.value.asInstanceOf[T])
      case Term.Block(Nil, e) => const(e)
      case Term.Inlined(_, Nil, e) => const(e)
      case _  => None
    }
    const(expr.reflect)
  }
}
