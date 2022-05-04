package i8577

import scala.quoted._

object MacroD:
  opaque type StringContext = scala.StringContext
  def apply(ctx: scala.StringContext): StringContext = ctx
  def unapply(ctx: StringContext): Option[scala.StringContext] = Some(ctx)

def implUnapplyD[T](sc: Expr[MacroD.StringContext], input: Expr[T])
                   (using Type[T])(using Quotes): Expr[Option[Seq[T]]] =
  '{ Some(Seq(${input})) }
