package i8577

import scala.quoted._

object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): Option[StringContext] = Some(ctx)

def implUnapply[T](sc: Expr[Macro.StrCtx], input: Expr[T])(using Type[T])(using Quotes): Expr[Option[Seq[T]]] =
  '{ Some(Seq(${input})) }
