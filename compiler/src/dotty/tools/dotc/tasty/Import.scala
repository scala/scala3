package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos
import dotty.tools.dotc.core.StdNames.nme


object Import {

  def apply(tree: Tree)(implicit ctx: Context): scala.tasty.Import = Impl(tree, ctx)

  object Import {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, List[scala.tasty.ImportSelector])] = term match {
      case Impl(Trees.Import(expr, selectors), ctx) => Some(Term(expr)(ctx), selectors.map(importSelector(_)(ctx)))
      case _ => None
    }
  }

  private def importSelector(tree: untpd.Tree)(implicit ctx: Context): scala.tasty.ImportSelector = tree match {
    case id@Trees.Ident(_) => scala.tasty.ImportSelector.Simple(Id(id))
    case Trees.Thicket((id@Trees.Ident(_)) :: Trees.Ident(nme.WILDCARD) :: Nil) => scala.tasty.ImportSelector.Omit(Id(id))
    case Trees.Thicket((id1@Trees.Ident(_)) :: (id2@Trees.Ident(_)) :: Nil) => scala.tasty.ImportSelector.Rename(Id(id1), Id(id2))
  }

  private case class Impl(tree: Tree, ctx: Context) extends scala.tasty.Import {

    implicit def ctx_ : Context = ctx

    def pos: scala.tasty.Position = new dotty.tools.dotc.tasty.Position(tree.pos)

    override def toString: String = this match {
      case Import(pkg, body) => s"Import($pkg, $body)"
      case _ => s"Import"
    }
  }

}
