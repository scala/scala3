package i8577

import scala.quoted._

object Macro:
  opaque type StrCtx = StringContext
  def apply(ctx: StringContext): StrCtx = ctx
  def unapply(ctx: StrCtx): Option[StringContext] = Some(ctx)

def implUnapply[T, U](sc: Expr[Macro.StrCtx], input: Expr[(T, U)])(using Type[T], Type[U])(using Quotes): Expr[Option[Seq[(T, U)]]] =
  '{ Some(Seq(${input})) }
