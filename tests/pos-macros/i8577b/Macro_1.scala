package i8577

import scala.quoted._

object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): Option[StringContext] = Some(ctx)

def implUnapply[U](sc: Expr[Macro.StrCtx], input: Expr[U])(using Type[U])(using Quotes): Expr[Option[Seq[U]]] =
  '{ Some(Seq(${input})) }
