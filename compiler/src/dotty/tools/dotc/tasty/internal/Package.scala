package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.statements

object Package {

  // TODO make sure all extractors are tested

  def apply(tree: tpd.Tree)(implicit ctx: Context): statements.Package = Impl(tree, ctx)

  def unapplyPackage(tree: scala.tasty.Tree): Option[statements.Package.Data] = tree match {
    case Impl(Trees.PackageDef(pkg, body), ctx) => Some(Term(pkg)(ctx), body.map(TopLevelStatement(_)(ctx)))
    case _ => None
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends statements.Package with Positioned {
    override def toString: String = {
      import Toolbox.extractor
      val statements.Package(pkg, body) = this
      s"Package($pkg, $body)"
    }
  }
}
