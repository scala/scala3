package i8577

import scala.quoted._

object MacroG:
  opaque type StringContext = scala.StringContext
  def apply(ctx: scala.StringContext): StringContext = ctx
  def unapply(ctx: StringContext): Option[scala.StringContext] = Some(ctx)

def implUnapplyG[T, U](sc: Expr[MacroG.StringContext], input: Expr[T | U])
                      (using Type[T], Type[U])(using Quotes): Expr[Option[Seq[T | U]]] =
  '{ Some(Seq(${input})) }
