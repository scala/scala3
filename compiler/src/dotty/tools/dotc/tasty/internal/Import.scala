package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme

import scala.tasty.statement
import scala.tasty.statement.Import.ImportSelector

object Import {

  def apply(tree: tpd.Tree)(implicit ctx: Context): statement.Import = Impl(tree, ctx)

  object Import {
    def unapply(term: statement.TopLevelStatement): Option[statement.Import.Data] = term match {
      case Impl(Trees.Import(expr, selectors), ctx) => Some(Term(expr)(ctx), selectors.map(importSelector(_)(ctx)))
      case _ => None
    }
  }

  private def importSelector(tree: untpd.Tree)(implicit ctx: Context): ImportSelector = tree match {
    case id@Trees.Ident(_) => ImportSelector.Simple(Id(id))
    case Trees.Thicket((id@Trees.Ident(_)) :: Trees.Ident(nme.WILDCARD) :: Nil) => ImportSelector.Omit(Id(id))
    case Trees.Thicket((id1@Trees.Ident(_)) :: (id2@Trees.Ident(_)) :: Nil) => ImportSelector.Rename(Id(id1), Id(id2))
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends statement.Import with Positioned {
    override def toString: String = this match {
      case Import(pkg, body) => s"Import($pkg, $body)"
      case _ => s"Import"
    }
  }

}
