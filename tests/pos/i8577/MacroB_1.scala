package i8577

import scala.quoted._

object MacroB:
  opaque type StringContext = scala.StringContext
  def apply(ctx: scala.StringContext): StringContext = ctx
  def unapply(ctx: StringContext): Option[scala.StringContext] = Some(ctx)

def implUnapplyB[U](sc: Expr[MacroB.StringContext], input: Expr[U])
                   (using Type[U])(using Quotes): Expr[Option[Seq[U]]] =
  '{ Some(Seq(${input})) }
