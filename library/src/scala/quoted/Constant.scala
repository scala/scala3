package scala.quoted

import scala.runtime.quoted
import scala.runtime.tasty

import scala.tasty.trees._
import scala.tasty.Context

object Constant {
  def unapply[T](expr: Expr[T])(implicit qtools: quoted.Toolbox[T]): Option[T] = {
    def const(tree: Tree)(implicit ctx: Context): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => const(e)
      case Inlined(_, Nil, e) => const(e)
      case _  => None
    }
    val (tree, ctx) = expr.toTasty
    const(tree)(ctx)
  }
}
