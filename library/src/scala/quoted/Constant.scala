package scala.quoted

import scala.runtime.quoted
import scala.runtime.tasty

import scala.tasty.trees._

object Constant {
  def unapply[T](expr: Expr[T])(implicit qtools: quoted.Toolbox[T], ttools: tasty.Toolbox): Option[T] = {
    def const(tree: Tree): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => const(e)
      case Inlined(_, Nil, e) => const(e)
      case _  => None
    }
    const(expr.toTasty)
  }
}
