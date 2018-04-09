package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names

import scala.tasty.trees
import scala.tasty.types

object TypeTree {

  def apply(tree: tpd.Tree): trees.TypeTree = new Impl(tree)

  def unapplySynthetic(arg: Impl)(implicit ctx: Context): Boolean = arg.tree match {
    case Trees.TypeTree() => true
    case _ => false
  }

  def unapplyTypeIdent(arg: Impl)(implicit ctx: Context): Option[trees.TypeIdent.Data] = arg.tree match {
    case id@Trees.Ident(name: Names.TypeName) if id.isType => Some(TypeName(name))
    case _ => None
  }

  def unapplyTypeSelect(arg: Impl)(implicit ctx: Context): Option[trees.TypeSelect.Data] = arg.tree match {
    case id@Trees.Select(qual, name: Names.TypeName) if id.isType => Some(Term(qual), TypeName(name))
    case _ => None
  }

  def unapplySingleton(arg: Impl)(implicit ctx: Context): Option[trees.Singleton.Data] = arg.tree match {
    case Trees.SingletonTypeTree(ref) => Some(Term(ref))
    case _ => None
  }

  def unapplyRefined(arg: Impl)(implicit ctx: Context): Option[trees.Refined.Data] = arg.tree match {
    case Trees.RefinedTypeTree(tpt, refinements) => Some(TypeTree(tpt), refinements.map(Definition(_)))
    case _ => None
  }

  def unapplyApplied(arg: Impl)(implicit ctx: Context): Option[trees.Applied.Data] = arg.tree match {
    case Trees.AppliedTypeTree(tycon, args) => Some(TypeTree(tycon), args.map(TypeTree(_)))
    case _ => None
  }

  def unapplyAnnotated(arg: Impl)(implicit ctx: Context): Option[trees.Annotated.Data] = arg.tree match {
    case Trees.Annotated(argument, annot) => Some(TypeTree(argument), Term(annot))
    case _ => None
  }

  def unapplyAnd(arg: Impl)(implicit ctx: Context): Option[trees.And.Data] = arg.tree match {
    case Trees.AndTypeTree(left, right) => Some(TypeTree(left), TypeTree(right))
    case _ => None
  }

  def unapplyOr(arg: Impl)(implicit ctx: Context): Option[trees.Or.Data] = arg.tree match {
    case Trees.OrTypeTree(left, right) => Some(TypeTree(left), TypeTree(right))
    case _ => None
  }

  def unapplyByName(arg: Impl)(implicit ctx: Context): Option[trees.ByName.Data] = arg.tree match {
    case Trees.ByNameTypeTree(tpt) => Some(TypeTree(tpt))
    case _ => None
  }

  def tree(tpe: trees.TypeTree)(implicit ctx: Context): tpd.Tree = tpe.asInstanceOf[Impl].tree

  private[tasty] class Impl(val tree: tpd.Tree) extends trees.TypeTree with Positioned {

    assert(!tree.isInstanceOf[Trees.TypeBoundsTree[_]])

    def tpe: types.Type = Type(tree.tpe)

    override def toString: String = "TypeTree"
  }
}
