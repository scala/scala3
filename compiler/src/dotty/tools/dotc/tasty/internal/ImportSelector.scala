package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme

import scala.tasty.trees

object ImportSelector {

  def apply(tree: untpd.Tree): trees.ImportSelector = new Impl(tree)

  def unapplySimpleSelector(arg: Impl)(implicit ctx: Context): Option[trees.SimpleSelector.Data] = arg.tree match {
    case id@Trees.Ident(_) => Some(Id(id))
    case _ => None
  }

  def unapplyRenameSelector(arg: Impl)(implicit ctx: Context): Option[trees.RenameSelector.Data] = arg.tree match {
    case Trees.Thicket((id1@Trees.Ident(_)) :: (id2@Trees.Ident(_)) :: Nil) if id2.name != nme.WILDCARD =>
      Some(Id(id1), Id(id2))
    case _ => None
  }

  def unapplyOmitSelector(arg: Impl)(implicit ctx: Context): Option[trees.OmitSelector.Data] = arg.tree match {
    case Trees.Thicket((id@Trees.Ident(_)) :: Trees.Ident(nme.WILDCARD) :: Nil) =>
      Some(Id(id))
    case _ => None
  }

  private[tasty] class Impl(val tree: untpd.Tree) extends trees.ImportSelector {
    override def toString: String = "ImportSelector"
  }

}
