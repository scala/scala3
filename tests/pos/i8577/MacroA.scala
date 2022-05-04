package i8577

import scala.quoted._

object MacroA:
  opaque type SC = scala.StringContext
  def apply(ctx: scala.StringContext): SC = ctx
  def unapply(ctx: SC): Option[scala.StringContext] = Some(ctx)

extension (ctx: StringContext) def mac: MacroA.SC = MacroA(ctx)
extension (inline ctx: MacroA.SC) inline def apply(inline args: Int*): String = ""
extension (inline ctx: MacroA.SC) inline def unapplySeq(inline input: Int): Option[Seq[Int]] =
  ${ implUnapply('ctx, 'input) }

def implUnapply(sc: Expr[MacroA.SC], input: Expr[Int])(using Quotes): Expr[Option[Seq[Int]]] =
  Expr(Some(Seq(0)))
