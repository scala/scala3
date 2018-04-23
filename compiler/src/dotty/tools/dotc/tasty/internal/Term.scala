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

  def apply(arg: tpd.Tree)(implicit ctx: Context): trees.Term = new Impl(arg)

  def unapplyIdent(arg: Impl): Option[trees.Ident.Data] = arg.tree match {
    case Trees.Ident(name: Names.TermName) if arg.tree.isTerm =>
      implicit val ctx: Context = arg.ctx
      Some(TermName(name))
    case _ => None
  }

  def unapplySelect(arg: Impl): Option[trees.Select.Data] = arg.tree match {
    case id@Trees.Select(qual, name: Names.TermName) if id.isTerm =>
      implicit val ctx: Context = arg.ctx
      Some(Term(qual), PossiblySignedName(name))
    case _ => None
  }

  def unapplyLiteral(arg: Impl): Option[trees.Literal.Data] = arg.tree match {
    case Trees.Literal(const) =>
      implicit val ctx: Context = arg.ctx
      Some(Constant(const))
    case _ => None
  }

  def unapplyThis(arg: Impl): Option[trees.This.Data] = arg.tree match {
    case Trees.This(qual) =>
      implicit val ctx: Context = arg.ctx
      Some(if (qual.isEmpty) None else Some(Id(qual)))
    case _ => None
  }

  def unapplyNew(arg: Impl): Option[trees.New.Data] = arg.tree match {
    case Trees.New(tpt) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeTree(tpt))
    case _ => None
  }

  def unapplyNamedArg(arg: Impl): Option[trees.NamedArg.Data] = arg.tree match {
    case Trees.NamedArg(name: Names.TermName, argument) =>
      implicit val ctx: Context = arg.ctx
      Some(TermName(name), Term(argument))
    case _ => None
  }

  def unapplyApply(arg: Impl): Option[trees.Apply.Data] = arg.tree match {
    case Trees.Apply(fn, args) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(fn), args.map(arg => Term(arg))))
    case _ => None
  }

  def unapplyTypeApply(arg: Impl): Option[trees.TypeApply.Data] = arg.tree match {
    case Trees.TypeApply(fn, args) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(fn), args.map(arg => TypeTree(arg))))
    case _ => None
  }

  def unapplySuper(arg: Impl): Option[trees.Super.Data] = arg.tree match {
    case Trees.Super(qual, mixin) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(qual), if (mixin.isEmpty) None else Some(Id(mixin))))
    case _ => None
  }

  def unapplyTyped(arg: Impl): Option[trees.Typed.Data] = arg.tree match {
    case Trees.Typed(expr, tpt) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(expr), TypeTree(tpt)))
    case _ => None
  }

  def unapplyAssign(arg: Impl): Option[trees.Assign.Data] = arg.tree match {
    case Trees.Assign(lhs, rhs) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(lhs), Term(rhs)))
    case _ => None
  }

  def unapplyBlock(arg: Impl): Option[trees.Block.Data] = arg.tree match {
    case Trees.Block(stats, expr) =>
      implicit val ctx: Context = arg.ctx
      Some((stats.map(stat => Statement(stat)), Term(expr)))
    case _ => None
  }

  def unapplyInlined(arg: Impl): Option[trees.Inlined.Data] = arg.tree match {
    case Trees.Inlined(call, bindings, expansion) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(call), bindings.map(Definition(_)), Term(expansion)))
    case _ => None
  }

  def unapplyLambda(arg: Impl): Option[trees.Lambda.Data] = arg.tree match {
    case Trees.Closure(_, meth, tpt) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(meth), if (tpt.isEmpty) None else Some(TypeTree(tpt))))
    case _ => None
  }

  def unapplyIf(arg: Impl): Option[trees.If.Data] = arg.tree match {
    case Trees.If(cond, thenp, elsep) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(cond), Term(thenp), Term(elsep)))
    case _ => None
  }

  def unapplyMatch(arg: Impl): Option[trees.Match.Data] = arg.tree match {
    case Trees.Match(selector, cases) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(selector), cases.map(c => CaseDef(c))))
    case _ => None
  }

  def unapplyTry(arg: Impl): Option[trees.Try.Data] = arg.tree match {
    case Trees.Try(body, catches, finalizer) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(body), catches.map(c => CaseDef(c)), if (finalizer.isEmpty) None else Some(Term(finalizer))))
    case _ => None
  }

  def unapplyReturn(arg: Impl): Option[trees.Return.Data] = arg.tree match {
    case Trees.Return(expr, from) =>
      implicit val ctx: Context = arg.ctx
      Some(Term(expr)) // TODO use `from` or remove it
    case _ => None
  }

  def unapplyRepeated(arg: Impl): Option[trees.Repeated.Data] = arg.tree match {
    case Trees.SeqLiteral(args, elemtpt) =>
      implicit val ctx: Context = arg.ctx
      Some(args.map(arg => Term(arg))) // TODO use `elemtpt`?
    case _ => None
  }

  def unapplySelectOuter(arg: Impl): Option[trees.SelectOuter.Data] = arg.tree match {
    case sel@Trees.Select(qual, OuterSelectName(_, levels)) =>
      implicit val ctx: Context = arg.ctx
      Some((Term(qual), levels, Type(sel.tpe)))
    case _ => None
  }

  def tree(term: trees.Term): tpd.Tree = term.asInstanceOf[Impl].tree

  private[tasty] class Impl(val tree: tpd.Tree)(implicit val ctx: Context) extends trees.Term with Positioned {

    assert(tree.isTerm || tree.isInstanceOf[Trees.NamedArg[_]] || tree.isInstanceOf[Trees.SeqLiteral[_]])

    def tpe: types.Type = Type(tree.tpe)

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case trees.Ident(name) => s"Ident($name)"
        case trees.Select(qual, name) => s"Select($qual, $name)"
        case trees.Literal(const) => s"Literal($const)"
        case trees.This(id) => s"This($id)"
        case trees.New(tpt) => s"New($tpt)"
        case trees.NamedArg(name, arg) => s"NamedArg($name, $arg)"
        case trees.Apply(fn, args) => s"Apply($fn, ${list(args)})"
        case trees.TypeApply(fn, args) => s"TypeApply($fn, ${list(args)})"
        case trees.Super(qual, mixin) => s"Super($qual, $mixin)"
        case trees.Typed(expr, tpt) => s"Typed($expr, $tpt)"
        case trees.Assign(lhs, rhs) => s"Assign($lhs, $rhs)"
        case trees.Block(stats, expr) => s"Block(${list(stats)}, $expr)"
        case trees.Inlined(call, bindings, expr) => s"Inlined($call, $bindings, $expr)"
        case trees.Lambda(meth, tpt) => s"Lambda($meth, $tpt)"
        case trees.If(cond, thenp, elsep) => s"If($cond, $thenp, $elsep)"
        case trees.Match(selector, cases) => s"Match($selector, ${list(cases)})"
        case trees.Try(body, catches, finalizer) => s"Try($body, ${list(catches)}, $finalizer)"
        case trees.Return(expr) => s"Return($expr)"
        case trees.Repeated(args) => s"Repeated($args)"
        case trees.SelectOuter(from, levels, target) => s"SelectOuter($from, $levels, $target)"
      }
    }

    private def list(xs: List[_]): String =
      if (xs.isEmpty) "Nil" else xs.mkString("List(", ", ", ")")
  }
}
