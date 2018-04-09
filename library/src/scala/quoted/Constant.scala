package scala.quoted

import scala.tasty.trees._
import scala.tasty.Context

object Constant {
  def unapply[T](expr: Expr[T])(implicit ctx: Context): Option[T] = {
    def const(tree: Tree): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => const(e)
      case Inlined(_, Nil, e) => const(e)
      case _  => None
    }
    const(expr.toTasty)
  }
}
