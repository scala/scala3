package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol

import scala.tasty
import scala.tasty.trees

object PackageDef {

  // TODO make sure all extractors are tested

  def apply(sym: Symbol): trees.PackageDef = new Impl(sym)

  def unapplyPackageDef(arg: Impl)(implicit ctx: Context): Option[trees.PackageDef.Data] = {
    Some(Name(arg.sym.name), Nil) // FIXME
  }

  private[tasty] class Impl(val sym: Symbol) extends trees.PackageDef {

    override def pos(implicit ctx: tasty.Context): tasty.Position = ??? // FIXME: A packageDef should not have a position, maybe Definition should not have positions

    def owner(implicit tctx: tasty.Context): trees.Definition = {
      implicit val ctx = tctx.asInstanceOf[TastyContext].ctx
      Definition(sym.owner)
    }

    def localContext(implicit tctx: tasty.Context): tasty.Context = {
      implicit val ctx = tctx.asInstanceOf[TastyContext].ctx
      new TastyContext(ctx.withOwner(sym))
    }

    override def toString: String = "PackageDef"
  }
}
