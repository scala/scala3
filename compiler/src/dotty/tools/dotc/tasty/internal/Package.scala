package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statement

object Package {

  def apply(tree: tpd.Tree)(implicit ctx: Context): statement.Package = Impl(tree, ctx)

  object Package {
    def unapply(term: scala.tasty.statement.TopLevelStatement): Option[statement.Package.Data] = term match {
      case Impl(Trees.PackageDef(pkg, body), ctx) => Some(Term(pkg)(ctx), body.map(TopLevelStatement(_)(ctx)))
      case _ => None
    }
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends statement.Package with Positioned {
    override def toString: String = this match {
      case Package(pkg, body) => s"Package($pkg, $body)"
      case _ => s"CaseDef"
    }
  }
}
