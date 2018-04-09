package dotty.tools.dotc.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos


object Package {

  def apply(tree: Tree)(implicit ctx: Context): scala.tasty.Package = Impl(tree, ctx)

  object Package {
    def unapply(term: scala.tasty.TopLevelStatement): Option[(scala.tasty.Term, List[scala.tasty.TopLevelStatement])] = term match {
      case Impl(Trees.PackageDef(pkg, body), ctx) => Some(Term(pkg)(ctx), body.map(TopLevelStatement(_)(ctx)))
      case _ => None
    }
  }

  private case class Impl(tree: Tree, ctx: Context) extends scala.tasty.Package {

    implicit def ctx_ : Context = ctx

    def pos: scala.tasty.Position = new dotty.tools.dotc.tasty.Position(tree.pos)

    override def toString: String = this match {
      case Package(pkg, body) => s"Package($pkg, $body)"
      case _ => s"CaseDef"
    }
  }
}
