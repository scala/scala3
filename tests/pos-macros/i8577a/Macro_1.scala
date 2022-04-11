package i8577

import scala.quoted._

object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): Option[StringContext] = Some(ctx)

def implUnapply(sc: Expr[Macro.StrCtx], input: Expr[Int])(using Quotes): Expr[Option[Seq[Int]]] =
  '{ Some(Seq(${input})) }
