package i8577

import scala.quoted._

object MacroE:
  opaque type StringContext = scala.StringContext
  def apply(ctx: scala.StringContext): StringContext = ctx
  def unapply(ctx: StringContext): Option[scala.StringContext] = Some(ctx)

def implUnapplyE[T, U](sc: Expr[MacroE.StringContext], input: Expr[U])
                      (using Type[U])(using Quotes): Expr[Option[Seq[U]]] =
  '{ Some(Seq(${input})) }
