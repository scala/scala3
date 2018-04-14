package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.typetrees
import scala.tasty.types

object TypeTree {

  def apply(tree: Tree)(implicit ctx: Context): typetrees.TypeTree = Impl(tree, ctx)

  def unapplySynthetic(tree: scala.tasty.Tree): Boolean = tree match {
    case Impl(Trees.TypeTree(), _) => true
    case _ => false
  }

  def unapplyTypeIdent(tree: scala.tasty.Tree): Option[typetrees.TypeIdent.Data] = tree match {
    case Impl(id@Trees.Ident(name: Names.TypeName), _) if id.isType => Some(TypeName(name))
    case _ => None
  }

  def unapplyTypeSelect(tree: scala.tasty.Tree): Option[typetrees.TypeSelect.Data] = tree match {
    case Impl(id@Trees.Select(qual, name: Names.TypeName), ctx) if id.isType => Some(Term(qual)(ctx), TypeName(name))
    case _ => None
  }

  def unapplySingleton(tree: scala.tasty.Tree): Option[typetrees.Singleton.Data] = tree match {
    case Impl(Trees.SingletonTypeTree(ref), ctx) => Some(Term(ref)(ctx))
    case _ => None
  }

  def unapplyRefined(tree: scala.tasty.Tree): Option[typetrees.Refined.Data] = tree match {
    case Impl(Trees.RefinedTypeTree(tpt, refinements), ctx) => Some(TypeTree(tpt)(ctx), refinements.map(Definition(_)(ctx)))
    case _ => None
  }

  def unapplyApplied(tree: scala.tasty.Tree): Option[typetrees.Applied.Data] = tree match {
    case Impl(Trees.AppliedTypeTree(tycon, args), ctx) => Some(TypeTree(tycon)(ctx), args.map(TypeTree(_)(ctx)))
    case _ => None
  }

  def unapplyAnnotated(tree: scala.tasty.Tree): Option[typetrees.Annotated.Data] = tree match {
    case Impl(Trees.Annotated(arg, annot), ctx) => Some(TypeTree(arg)(ctx), Term(annot)(ctx))
    case _ => None
  }

  def unapplyAnd(tree: scala.tasty.Tree): Option[typetrees.And.Data] = tree match {
    case Impl(Trees.AndTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
    case _ => None
  }

  def unapplyOr(tree: scala.tasty.Tree): Option[typetrees.Or.Data] = tree match {
    case Impl(Trees.OrTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
    case _ => None
  }

  def unapplyByName(tree: scala.tasty.Tree): Option[typetrees.ByName.Data] = tree match {
    case Impl(Trees.ByNameTypeTree(tpt), ctx) => Some(TypeTree(tpt)(ctx))
    case _ => None
  }

  private case class Impl(tree: Tree, ctx: Context) extends typetrees.TypeTree with Positioned {

    assert(!tree.isInstanceOf[Trees.TypeBoundsTree[_]])

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case typetrees.Synthetic() => "Synthetic()"
        case typetrees.TypeIdent(name) => s"Ident($name)"
        case typetrees.TypeSelect(qual, name) => s"Select($qual, $name)"
        case typetrees.Singleton(ref) => s"Singleton($ref)"
        case typetrees.Refined(underlying, refinements) => s"Refined($underlying, ${list(refinements)})"
        case typetrees.Applied(tycon, args) => s"Applied($tycon, $args)"
        case typetrees.TypeBoundsTree(lo, hi) => s"TypeBoundsTree($lo, $hi)"
        case typetrees.Annotated(arg, annot) => s"Annotated($arg, $annot)"
        case typetrees.And(left, right) => s"And($left, $right)"
        case typetrees.Or(left, right) => s"Or($left, $right)"
        case typetrees.ByName(tpt) => s"ByName($tpt)"
      }
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}
