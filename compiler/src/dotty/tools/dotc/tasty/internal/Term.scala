package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.Names

import scala.tasty.trees
import scala.tasty.types

object Term {

  // TODO make sure all extractors are tested

  def apply(arg: tpd.Tree): trees.Term = new Impl(arg)

  def unapplyIdent(arg: Impl)(implicit ctx: Context): Option[trees.Ident.Data] = arg.tree match {
    case Trees.Ident(name: Names.TermName) if arg.tree.isTerm => Some(TermName(name))
    case _ => None
  }

  def unapplySelect(arg: Impl)(implicit ctx: Context): Option[trees.Select.Data] = arg.tree match {
    case id@Trees.Select(qual, name: Names.TermName) if id.isTerm => Some(Term(qual), PossiblySignedName(name))
    case _ => None
  }

  def unapplyLiteral(arg: Impl)(implicit ctx: Context): Option[trees.Literal.Data] = arg.tree match {
    case Trees.Literal(const) => Some(Constant(const))
    case _ => None
  }

  def unapplyThis(arg: Impl)(implicit ctx: Context): Option[trees.This.Data] = arg.tree match {
    case Trees.This(qual) => Some(if (qual.isEmpty) None else Some(Id(qual)))
    case _ => None
  }

  def unapplyNew(arg: Impl)(implicit ctx: Context): Option[trees.New.Data] = arg.tree match {
    case Trees.New(tpt) => Some(TypeTree(tpt))
    case _ => None
  }

  def unapplyNamedArg(arg: Impl)(implicit ctx: Context): Option[trees.NamedArg.Data] = arg.tree match {
    case Trees.NamedArg(name: Names.TermName, argument) => Some(TermName(name), Term(argument))
    case _ => None
  }

  def unapplyApply(arg: Impl)(implicit ctx: Context): Option[trees.Apply.Data] = arg.tree match {
    case Trees.Apply(fn, args) => Some((Term(fn), args.map(arg => Term(arg))))
    case _ => None
  }

  def unapplyTypeApply(arg: Impl)(implicit ctx: Context): Option[trees.TypeApply.Data] = arg.tree match {
    case Trees.TypeApply(fn, args) => Some((Term(fn), args.map(arg => TypeTree(arg))))
    case _ => None
  }

  def unapplySuper(arg: Impl)(implicit ctx: Context): Option[trees.Super.Data] = arg.tree match {
    case Trees.Super(qual, mixin) => Some((Term(qual), if (mixin.isEmpty) None else Some(Id(mixin))))
    case _ => None
  }

  def unapplyTyped(arg: Impl)(implicit ctx: Context): Option[trees.Typed.Data] = arg.tree match {
    case Trees.Typed(expr, tpt) => Some((Term(expr), TypeTree(tpt)))
    case _ => None
  }

  def unapplyAssign(arg: Impl)(implicit ctx: Context): Option[trees.Assign.Data] = arg.tree match {
    case Trees.Assign(lhs, rhs) => Some((Term(lhs), Term(rhs)))
    case _ => None
  }

  def unapplyBlock(arg: Impl)(implicit ctx: Context): Option[trees.Block.Data] = arg.tree match {
    case Trees.Block(stats, expr) => Some((stats.map(stat => Statement(stat)), Term(expr)))
    case _ => None
  }

  def unapplyInlined(arg: Impl)(implicit ctx: Context): Option[trees.Inlined.Data] = arg.tree match {
    case Trees.Inlined(call, bindings, expansion) =>
      Some((Term(call), bindings.map(Definition(_)), Term(expansion)))
    case _ => None
  }

  def unapplyLambda(arg: Impl)(implicit ctx: Context): Option[trees.Lambda.Data] = arg.tree match {
    case Trees.Closure(_, meth, tpt) => Some((Term(meth), if (tpt.isEmpty) None else Some(TypeTree(tpt))))
    case _ => None
  }

  def unapplyIf(arg: Impl)(implicit ctx: Context): Option[trees.If.Data] = arg.tree match {
    case Trees.If(cond, thenp, elsep) => Some((Term(cond), Term(thenp), Term(elsep)))
    case _ => None
  }

  def unapplyMatch(arg: Impl)(implicit ctx: Context): Option[trees.Match.Data] = arg.tree match {
    case Trees.Match(selector, cases) => Some((Term(selector), cases.map(c => CaseDef(c))))
    case _ => None
  }

  def unapplyTry(arg: Impl)(implicit ctx: Context): Option[trees.Try.Data] = arg.tree match {
    case Trees.Try(body, catches, finalizer) => Some((Term(body), catches.map(c => CaseDef(c)), if (finalizer.isEmpty) None else Some(Term(finalizer))))
    case _ => None
  }

  def unapplyReturn(arg: Impl)(implicit ctx: Context): Option[trees.Return.Data] = arg.tree match {
    case Trees.Return(expr, from) => Some(Term(expr)) // TODO use `from` or remove it
    case _ => None
  }

  def unapplyRepeated(arg: Impl)(implicit ctx: Context): Option[trees.Repeated.Data] = arg.tree match {
    case Trees.SeqLiteral(args, elemtpt) => Some(args.map(arg => Term(arg))) // TODO use `elemtpt`?
    case _ => None
  }

  def unapplySelectOuter(arg: Impl)(implicit ctx: Context): Option[trees.SelectOuter.Data] = arg.tree match {
    case sel@Trees.Select(qual, OuterSelectName(_, levels)) => Some((Term(qual), levels, Type(sel.tpe)))
    case _ => None
  }

  def tree(term: trees.Term): tpd.Tree = term.asInstanceOf[Impl].tree

  private[tasty] class Impl(val tree: tpd.Tree) extends trees.Term with Positioned {

    assert(tree.isTerm || tree.isInstanceOf[Trees.NamedArg[_]] || tree.isInstanceOf[Trees.SeqLiteral[_]])

    def tpe: types.Type = Type(tree.tpe)

    override def toString: String = "Term"
  }
}
