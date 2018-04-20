package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme

import scala.tasty.trees

object ImportSelector {

  def apply(tree: untpd.Tree)(implicit ctx: Context): trees.ImportSelector = new Impl(tree)

  def unapplySimpleSelector(arg: Impl): Option[trees.SimpleSelector.Data] = arg.tree match {
    case id@Trees.Ident(_) =>
      implicit val ctx: Context = arg.ctx
      Some(Id(id))
    case _ => None
  }

  def unapplyRenameSelector(arg: Impl): Option[trees.RenameSelector.Data] = arg.tree match {
    case Trees.Thicket((id1@Trees.Ident(_)) :: (id2@Trees.Ident(_)) :: Nil) if id2.name != nme.WILDCARD =>
      implicit val ctx: Context = arg.ctx
      Some(Id(id1), Id(id2))
    case _ => None
  }

  def unapplyOmitSelector(arg: Impl): Option[trees.OmitSelector.Data] = arg.tree match {
    case Trees.Thicket((id@Trees.Ident(_)) :: Trees.Ident(nme.WILDCARD) :: Nil) =>
      implicit val ctx: Context = arg.ctx
      Some(Id(id))
    case _ => None
  }

  private[tasty] class Impl(val tree: untpd.Tree)(implicit val ctx: Context) extends trees.ImportSelector {
    override def toString: String = {
      import Toolbox.extractor
      this match {
        case trees.SimpleSelector(id) => s"SimpleSelector($id)"
        case trees.RenameSelector(id1, id2) => s"RenameSelector($id1, $id2)"
        case trees.OmitSelector(id) => s"OmitSelector($id)"
      }
    }
  }

}
