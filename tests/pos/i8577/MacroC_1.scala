package i8577

import scala.quoted._

object MacroC:
  opaque type StringContext = scala.StringContext
  def apply(ctx: scala.StringContext): StringContext = ctx
  def unapply(ctx: StringContext): Option[scala.StringContext] = Some(ctx)

def implUnapplyC[T](sc: Expr[MacroC.StringContext], input: Expr[T])
                   (using Type[T])(using Quotes): Expr[Option[Seq[T]]] =
  '{ Some(Seq(${input})) }
