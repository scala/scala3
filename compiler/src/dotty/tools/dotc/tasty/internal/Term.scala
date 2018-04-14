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

  def apply(arg: tpd.Tree)(implicit ctx: Context): trees.Term = Impl(arg, ctx)

  def unapplyIdent(tree: scala.tasty.Tree): Option[trees.Ident.Data] = tree match {
    case Impl(id@Trees.Ident(name: Names.TermName), _) if id.isTerm => Some(TermName(name))
    case _ => None
  }

  def unapplySelect(tree: scala.tasty.Tree): Option[trees.Select.Data] = tree match {
    case Impl(id@Trees.Select(qual, name: Names.TermName), ctx) if id.isTerm => Some(Term(qual)(ctx), TermName(name))
    case _ => None
  }

  def unapplyLiteral(tree: scala.tasty.Tree): Option[trees.Literal.Data] = tree match {
    case Impl(Trees.Literal(const), _) => Some(Constant(const))
    case _ => None
  }

  def unapplyThis(tree: scala.tasty.Tree): Option[trees.This.Data] = tree match {
    case Impl(Trees.This(qual), ctx) => Some(if (qual.isEmpty) None else Some(Id(qual)(ctx)))
    case _ => None
  }

  def unapplyNew(tree: scala.tasty.Tree): Option[trees.New.Data] = tree match {
    case Impl(Trees.New(tpt), ctx) => Some(TypeTree(tpt)(ctx))
    case _ => None
  }

  def unapplyNamedArg(tree: scala.tasty.Tree): Option[trees.NamedArg.Data] = tree match {
    case Impl(Trees.NamedArg(name: Names.TermName, arg), ctx) => Some(TermName(name), Term(arg)(ctx))
    case _ => None
  }

  def unapplyApply(tree: scala.tasty.Tree): Option[trees.Apply.Data] = tree match {
    case Impl(Trees.Apply(fn, args), ctx) => Some((Term(fn)(ctx), args.map(arg => Term(arg)(ctx))))
    case _ => None
  }

  def unapplyTypeApply(tree: scala.tasty.Tree): Option[trees.TypeApply.Data] = tree match {
    case Impl(Trees.TypeApply(fn, args), ctx) => Some((Term(fn)(ctx), args.map(arg => TypeTree(arg)(ctx))))
    case _ => None
  }

  def unapplySuper(tree: scala.tasty.Tree): Option[trees.Super.Data] = tree match {
    case Impl(Trees.Super(qual, mixin), ctx) => Some((Term(qual)(ctx), if (mixin.isEmpty) None else Some(Id(mixin)(ctx))))
    case _ => None
  }

  def unapplyTyped(tree: scala.tasty.Tree): Option[trees.Typed.Data] = tree match {
    case Impl(Trees.Typed(expr, tpt), ctx) => Some((Term(expr)(ctx), TypeTree(tpt)(ctx)))
    case _ => None
  }

  def unapplyAssign(tree: scala.tasty.Tree): Option[trees.Assign.Data] = tree match {
    case Impl(Trees.Assign(lhs, rhs), ctx) => Some((Term(lhs)(ctx), Term(rhs)(ctx)))
    case _ => None
  }

  def unapplyBlock(tree: scala.tasty.Tree): Option[trees.Block.Data] = tree match {
    case Impl(Trees.Block(stats, expr), ctx) => Some((stats.map(stat => Statement(stat)(ctx)), Term(expr)(ctx)))
    case _ => None
  }

  def unapplyInlined(tree: scala.tasty.Tree): Option[trees.Inlined.Data] = tree match {
    case Impl(Trees.Inlined(call, bindings, expansion), ctx) => Some((Term(call)(ctx), bindings.map(Definition(_)(ctx)), Term(expansion)(ctx)))
    case _ => None
  }

  def unapplyLambda(tree: scala.tasty.Tree): Option[trees.Lambda.Data] = tree match {
    case Impl(Trees.Closure(_, meth, tpt), ctx) => Some((Term(meth)(ctx), if (tpt.isEmpty) None else Some(TypeTree(tpt)(ctx))))
    case _ => None
  }

  def unapplyIf(tree: scala.tasty.Tree): Option[trees.If.Data] = tree match {
    case Impl(Trees.If(cond, thenp, elsep), ctx) => Some((Term(cond)(ctx), Term(thenp)(ctx), Term(elsep)(ctx)))
    case _ => None
  }

  def unapplyMatch(tree: scala.tasty.Tree): Option[trees.Match.Data] = tree match {
    case Impl(Trees.Match(selector, cases), ctx) => Some((Term(selector)(ctx), cases.map(c => CaseDef(c)(ctx))))
    case _ => None
  }

  def unapplyTry(tree: scala.tasty.Tree): Option[trees.Try.Data] = tree match {
    case Impl(Trees.Try(body, catches, finalizer), ctx) => Some((Term(body)(ctx), catches.map(c => CaseDef(c)(ctx)), if (finalizer.isEmpty) None else Some(Term(finalizer)(ctx))))
    case _ => None
  }

  def unapplyReturn(tree: scala.tasty.Tree): Option[trees.Return.Data] = tree match {
    case Impl(Trees.Return(expr, from), ctx) => Some(Term(expr)(ctx)) // TODO use `from` or remove it
    case _ => None
  }

  def unapplyRepeated(tree: scala.tasty.Tree): Option[trees.Repeated.Data] = tree match {
    case Impl(Trees.SeqLiteral(args, elemtpt), ctx) => Some(args.map(arg => Term(arg)(ctx))) // TODO use `elemtpt`?
    case _ => None
  }

  def unapplySelectOuter(tree: scala.tasty.Tree): Option[trees.SelectOuter.Data] = tree match {
    case Impl(sel@Trees.Select(qual, OuterSelectName(_, levels)), ctx) => Some((Term(qual)(ctx), levels, Type(sel.tpe)(ctx)))
    case _ => None
  }

  private case class Impl(tree: tpd.Tree, ctx: Context) extends trees.Term with Positioned {

    assert(tree.isTerm || tree.isInstanceOf[Trees.NamedArg[_]] || tree.isInstanceOf[Trees.SeqLiteral[_]])

    def tpe: types.Type = Type(tree.tpe)(ctx)

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
