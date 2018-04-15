package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.trees
import scala.tasty.types

object TypeTree {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.TypeTree = Impl(tree, ctx)

  def unapplySynthetic(tree: trees.Tree): Boolean = tree match {
    case Impl(Trees.TypeTree(), _) => true
    case _ => false
  }

  def unapplyTypeIdent(tree: trees.Tree): Option[trees.TypeIdent.Data] = tree match {
    case Impl(id@Trees.Ident(name: Names.TypeName), _) if id.isType => Some(TypeName(name))
    case _ => None
  }

  def unapplyTypeSelect(tree: trees.Tree): Option[trees.TypeSelect.Data] = tree match {
    case Impl(id@Trees.Select(qual, name: Names.TypeName), ctx) if id.isType => Some(Term(qual)(ctx), TypeName(name))
    case _ => None
  }

  def unapplySingleton(tree: trees.Tree): Option[trees.Singleton.Data] = tree match {
    case Impl(Trees.SingletonTypeTree(ref), ctx) => Some(Term(ref)(ctx))
    case _ => None
  }

  def unapplyRefined(tree: trees.Tree): Option[trees.Refined.Data] = tree match {
    case Impl(Trees.RefinedTypeTree(tpt, refinements), ctx) => Some(TypeTree(tpt)(ctx), refinements.map(Definition(_)(ctx)))
    case _ => None
  }

  def unapplyApplied(tree: trees.Tree): Option[trees.Applied.Data] = tree match {
    case Impl(Trees.AppliedTypeTree(tycon, args), ctx) => Some(TypeTree(tycon)(ctx), args.map(TypeTree(_)(ctx)))
    case _ => None
  }

  def unapplyAnnotated(tree: trees.Tree): Option[trees.Annotated.Data] = tree match {
    case Impl(Trees.Annotated(arg, annot), ctx) => Some(TypeTree(arg)(ctx), Term(annot)(ctx))
    case _ => None
  }

  def unapplyAnd(tree: trees.Tree): Option[trees.And.Data] = tree match {
    case Impl(Trees.AndTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
    case _ => None
  }

  def unapplyOr(tree: trees.Tree): Option[trees.Or.Data] = tree match {
    case Impl(Trees.OrTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
    case _ => None
  }

  def unapplyByName(tree: trees.Tree): Option[trees.ByName.Data] = tree match {
    case Impl(Trees.ByNameTypeTree(tpt), ctx) => Some(TypeTree(tpt)(ctx))
    case _ => None
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends trees.TypeTree with Positioned {

    assert(!tree.isInstanceOf[Trees.TypeBoundsTree[_]])

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case trees.Synthetic() => "Synthetic()"
        case trees.TypeIdent(name) => s"Ident($name)"
        case trees.TypeSelect(qual, name) => s"Select($qual, $name)"
        case trees.Singleton(ref) => s"Singleton($ref)"
        case trees.Refined(underlying, refinements) => s"Refined($underlying, ${list(refinements)})"
        case trees.Applied(tycon, args) => s"Applied($tycon, $args)"
        case trees.TypeBoundsTree(lo, hi) => s"TypeBoundsTree($lo, $hi)"
        case trees.Annotated(arg, annot) => s"Annotated($arg, $annot)"
        case trees.And(left, right) => s"And($left, $right)"
        case trees.Or(left, right) => s"Or($left, $right)"
        case trees.ByName(tpt) => s"ByName($tpt)"
      }
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}
