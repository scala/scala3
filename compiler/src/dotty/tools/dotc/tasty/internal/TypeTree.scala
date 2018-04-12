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

  // TODO remove objects

  object Synthetic {
    def unapply(arg: typetrees.TypeTree): Boolean = arg match {
      case Impl(Trees.TypeTree(), _) => true
      case _ => false
    }
  }

  object Ident {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.Ident.Data] = arg match {
      case Impl(id@Trees.Ident(name: Names.TypeName), _) if id.isType => Some(TypeName(name))
      case _ => None
    }
  }

  object Select {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.Select.Data] = arg match {
      case Impl(id@Trees.Select(qual, name: Names.TypeName), ctx) if id.isType => Some(Term(qual)(ctx), TypeName(name))
      case _ => None
    }
  }

  object Singleton {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.Singleton.Data] = arg match {
      case Impl(Trees.SingletonTypeTree(ref), ctx) => Some(Term(ref)(ctx))
      case _ => None
    }
  }

//  object Refined {
//    def unapply(arg: typetrees.TypeTree): Option[typetrees.Refined.Data] = arg match {
//      case Impl(Trees.RefinedTypeTree(tpt, refinements), ctx) => Some(TypeTree(tpt)(ctx), refinements.map(Definition(_)(ctx)))
//      case _ => None
//    }
//  }

  object Applied {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.Applied.Data] = arg match {
      case Impl(Trees.AppliedTypeTree(tycon, args), ctx) => Some(TypeTree(tycon)(ctx), args.map(TypeTree(_)(ctx)))
      case _ => None
    }
  }

  object TypeBounds {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.TypeBounds.Data] = arg match {
      case Impl(Trees.TypeBoundsTree(lo, hi), ctx) => Some(TypeTree(lo)(ctx), TypeTree(hi)(ctx))
      case _ => None
    }
  }

  object Annotated {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.Annotated.Data] = arg match {
      case Impl(Trees.Annotated(arg, annot), ctx) => Some(TypeTree(arg)(ctx), Term(annot)(ctx))
      case _ => None
    }
  }

  object And {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.And.Data] = arg match {
      case Impl(Trees.AndTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
      case _ => None
    }
  }

  object Or {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.Or.Data] = arg match {
      case Impl(Trees.OrTypeTree(left, right), ctx) => Some(TypeTree(left)(ctx), TypeTree(right)(ctx))
      case _ => None
    }
  }

  object ByName {
    def unapply(arg: typetrees.TypeTree): Option[typetrees.ByName.Data] = arg match {
      case Impl(Trees.ByNameTypeTree(tpt), ctx) => Some(TypeTree(tpt)(ctx))
      case _ => None
    }
  }

  private case class Impl(tree: Tree, ctx: Context) extends typetrees.TypeTree with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    override def toString: String = {
      import Toolbox.extractor
      this match {
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
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}
