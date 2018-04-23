package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol

import scala.tasty
import scala.tasty.trees

object PackageDef {

  // TODO make sure all extractors are tested

  def apply(sym: Symbol)(implicit ctx: Context): trees.PackageDef = new Impl(sym)

  def unapplyPackageDef(arg: Impl): Option[trees.PackageDef.Data] = {
    implicit val ctx: Context = arg.ctx
    val localContext = ctx.withOwner(arg.sym)
    Some(Name(arg.sym.name), Nil) // FIXME
  }

  private[tasty] class Impl(val sym: Symbol)(implicit val ctx: Context) extends trees.PackageDef {

    override def pos: tasty.Position = ??? // FIXME: A packageDef should not have a position, maybe Definition should not have positions

    override def owner: trees.Definition = Definition(sym.owner)

    override def toString: String = {
      import Toolbox.extractor
      val trees.PackageDef(name, members) = this
      s"PackageDef($name, $members)"
    }
  }
}
