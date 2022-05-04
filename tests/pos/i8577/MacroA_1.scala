package i8577

import scala.quoted._

object MacroA:
  opaque type StringContext = scala.StringContext
  def apply(ctx: scala.StringContext): StringContext = ctx
  def unapply(ctx: StringContext): Option[scala.StringContext] = Some(ctx)

def implUnapplyA(sc: Expr[MacroB.StringContext], input: Expr[Int])
                   (using Quotes): Expr[Option[Seq[Int]]] =
  '{ Some(Seq(${input})) }
