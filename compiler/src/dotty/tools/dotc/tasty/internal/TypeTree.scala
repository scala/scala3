package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.trees
import scala.tasty.types

object TypeTree {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.TypeTree = new Impl(tree)

  def unapplySynthetic(arg: Impl): Boolean = arg.tree match {
    case Trees.TypeTree() => true
    case _ => false
  }

  def unapplyTypeIdent(arg: Impl): Option[trees.TypeIdent.Data] = arg.tree match {
    case id@Trees.Ident(name: Names.TypeName) if id.isType => Some(TypeName(name))
    case _ => None
  }

  def unapplyTypeSelect(arg: Impl): Option[trees.TypeSelect.Data] = arg.tree match {
    case id@Trees.Select(qual, name: Names.TypeName) if id.isType =>
      implicit val ctx: Context = arg.ctx
      Some(Term(qual), TypeName(name))
    case _ => None
  }

  def unapplySingleton(arg: Impl): Option[trees.Singleton.Data] = arg.tree match {
    case Trees.SingletonTypeTree(ref) =>
      implicit val ctx: Context = arg.ctx
      Some(Term(ref)(ctx))
    case _ => None
  }

  def unapplyRefined(arg: Impl): Option[trees.Refined.Data] = arg.tree match {
    case Trees.RefinedTypeTree(tpt, refinements) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(tpt), refinements.map(Definition(_)))
    case _ => None
  }

  def unapplyApplied(arg: Impl): Option[trees.Applied.Data] = arg.tree match {
    case Trees.AppliedTypeTree(tycon, args) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(tycon), args.map(TypeTree(_)))
    case _ => None
  }

  def unapplyAnnotated(arg: Impl): Option[trees.Annotated.Data] = arg.tree match {
    case Trees.Annotated(argument, annot) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(argument), Term(annot))
    case _ => None
  }

  def unapplyAnd(arg: Impl): Option[trees.And.Data] = arg.tree match {
    case Trees.AndTypeTree(left, right) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(left), TypeTree(right))
    case _ => None
  }

  def unapplyOr(arg: Impl): Option[trees.Or.Data] = arg.tree match {
    case Trees.OrTypeTree(left, right) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(left), TypeTree(right))
    case _ => None
  }

  def unapplyByName(arg: Impl): Option[trees.ByName.Data] = arg.tree match {
    case Trees.ByNameTypeTree(tpt) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(tpt))
    case _ => None
  }

  def tree(tpe: trees.TypeTree): tpd.Tree = tpe.asInstanceOf[Impl].tree

  private[tasty] class Impl(val tree: tpd.Tree)(implicit val ctx: Context) extends trees.TypeTree with Positioned {

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
