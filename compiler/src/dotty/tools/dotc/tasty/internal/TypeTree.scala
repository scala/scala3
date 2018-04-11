package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.typetree

object TypeTree {

  def apply(tree: Tree)(implicit ctx: Context): typetree.TypeTree = Impl(tree, ctx)

  object Synthetic {
    def unapply(term: typetree.TypeTree): Boolean = term match {
      case Impl(Trees.TypeTree(), _) => true
      case _ => false
    }
  }

  object Ident {
    def unapply(term: typetree.TypeTree): Option[typetree.Ident.Data] = term match {
      case Impl(id@Trees.Ident(name: Names.TypeName), _) if id.isType => Some(TypeName(name))
      case _ => None
    }
  }

  object Select {
    def unapply(term: typetree.TypeTree): Option[typetree.Select.Data] = term match {
      case Impl(id@Trees.Select(qual, name: Names.TypeName), ctx) if id.isType => Some(Term(qual)(ctx), TypeName(name))
      case _ => None
    }
  }

  object Singleton {
    def unapply(term: typetree.TypeTree): Option[typetree.Singleton.Data] = term match {
      case Impl(Trees.SingletonTypeTree(ref), ctx) => Some(Term(ref)(ctx))
      case _ => None
    }
  }

//  object Refined {
//    def unapply(term: typetree.TypeTree): Option[typetree.Refined.Data] = term match {
//      case Impl(Trees.RefinedTypeTree(tpt, refinements), ctx) => Some(TypeTree(tpt)(ctx), refinements.map(Definition(_)(ctx)))
//      case _ => None
//    }
//  }

  object Applied {
    def unapply(term: typetree.TypeTree): Option[typetree.Applied.Data] = term match {
      case Impl(Trees.AppliedTypeTree(tycon, args), ctx) => Some(TypeTree(tycon)(ctx), args.map(TypeTree(_)(ctx)))
      case _ => None
    }
  }

  object TypeBounds {
    def unapply(term: typetree.TypeTree): Option[typetree.TypeBounds.Data] = term match {
      case Impl(Trees.TypeBoundsTree(lo, hi), ctx) => Some(TypeTree(lo)(ctx), TypeTree(hi)(ctx))
      case _ => None
    }
  }

  object Annotated {
    def unapply(term: typetree.TypeTree): Option[typetree.Annotated.Data] = term match {
      case Impl(Trees.Annotated(arg, annot), ctx) => Some(TypeTree(arg)(ctx), Term(annot)(ctx))
      case _ => None
    }
  }

  object And {
    def unapply(term: typetree.TypeTree): Option[typetree.And.Data] = term match {
      case Impl(Trees.AndTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
      case _ => None
    }
  }

  object Or {
    def unapply(term: typetree.TypeTree): Option[typetree.Or.Data] = term match {
      case Impl(Trees.OrTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
      case _ => None
    }
  }

  object ByName {
    def unapply(term: typetree.TypeTree): Option[typetree.ByName.Data] = term match {
      case Impl(Trees.ByNameTypeTree(tpt), ctx) => Some(TypeTree(tpt)(ctx))
      case _ => None
    }
  }

  private case class Impl(tree: Tree, ctx: Context) extends typetree.TypeTree with Positioned {

    def tpe: scala.tasty.Type = Type(tree.tpe)(ctx)

    override def toString: String = this match {
      case Synthetic() => "Synthetic()"
      case Ident(name) => s"Ident($name)"
      case Select(qual, name) => s"Select($qual, $name)"
      case Singleton(ref) => s"Singleton($ref)"
//      case Refined(tpt, refinements) => s"Refined($ref, ${list(refinements)})"
      case Applied(tycon, args) => s"Applied($tycon, $args)"
      case TypeBounds(lo, hi) => s"TypeBounds($lo, $hi)"
      case Annotated(arg, annot) => s"Annotated($arg, $annot)"
      case And(left, right) => s"And($left, $right)"
      case Or(left, right) => s"Or($left, $right)"
      case ByName(tpt) => s"ByName($tpt)"
      case _ => s"TypeTree{## $tree ##}"
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}
