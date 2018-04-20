package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme

import scala.tasty.trees

object ImportSelector {

  def apply(tree: untpd.Tree)(implicit ctx: Context): trees.ImportSelector = Impl(tree, ctx)

  def unapplySimpleSelector(tree: trees.ImportSelector): Option[trees.SimpleSelector.Data] = tree match {
    case Impl(id@Trees.Ident(_), ctx) => Some(Id(id)(ctx))
    case _ => None
  }

  def unapplyRenameSelector(tree: trees.ImportSelector): Option[trees.RenameSelector.Data] = tree match {
    case Impl(Trees.Thicket((id1@Trees.Ident(_)) :: (id2@Trees.Ident(_)) :: Nil), ctx) if id2.name != nme.WILDCARD => Some(Id(id1)(ctx), Id(id2)(ctx))
    case _ => None
  }

  def unapplyOmitSelector(tree: trees.ImportSelector): Option[trees.OmitSelector.Data] = tree match {
    case Impl(Trees.Thicket((id@Trees.Ident(_)) :: Trees.Ident(nme.WILDCARD) :: Nil), ctx) => Some(Id(id)(ctx))
    case _ => None
  }

  private case class Impl(tree: untpd.Tree, ctx: Context) extends trees.ImportSelector {
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
