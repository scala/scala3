package dotty.tools.dotc
package typer

import core._
import Contexts._
import Types._
import Symbols._
import StdNames._
import Decorators._
import typer.ProtoTypes._
import ast.{tpd, untpd, Trees}
import Trees._
import scala.util.control.NonFatal
import util.Spans.Span

/** A version of Typer that keeps all symbols defined and referenced in a
 *  previously typed tree.
 *
 *  All definition nodes keep their symbols. All leaf nodes for idents, selects,
 *  and TypeTrees keep their types. Indexing is a no-op.
 *
 *  Otherwise, everything is as in Typer.
 */
class ReTyper extends Typer with ReChecking {
  import tpd._

  private def assertTyped(tree: untpd.Tree)(implicit ctx: Context): Unit =
    assert(tree.hasType, i"$tree ${tree.getClass} ${tree.uniqueId}")

  /** Checks that the given tree has been typed */
  protected def promote(tree: untpd.Tree)(implicit ctx: Context): tree.ThisTree[Type] = {
    assertTyped(tree)
    tree.withType(tree.typeOpt)
  }

  override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree =
    promote(tree)

  override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
    assertTyped(tree)
    val qual1 = typed(tree.qualifier, AnySelectionProto)(ctx.retractMode(Mode.Pattern))
    untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
  }

  override def typedLiteral(tree: untpd.Literal)(implicit ctc: Context): Tree =
    promote(tree)

  override def typedThis(tree: untpd.This)(implicit ctx: Context): Tree =
    promote(tree)

  override def typedSuper(tree: untpd.Super, pt: Type)(implicit ctx: Context): Tree =
    promote(tree)

  override def typedTyped(tree: untpd.Typed, pt: Type)(implicit ctx: Context): Tree = {
    assertTyped(tree)
    val tpt1 = checkSimpleKinded(typedType(tree.tpt))
    val expr1 = tree.expr match {
      case id: untpd.Ident if (ctx.mode is Mode.Pattern) && untpd.isVarPattern(id) && (id.name == nme.WILDCARD || id.name == nme.WILDCARD_STAR) =>
        tree.expr.withType(tpt1.tpe)
      case _ => typed(tree.expr)
    }
   untpd.cpy.Typed(tree)(expr1, tpt1).withType(tree.typeOpt)
  }

  override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(implicit ctx: Context): TypeTree =
    promote(tree)

  override def typedRefinedTypeTree(tree: untpd.RefinedTypeTree)(implicit ctx: Context): TypTree =
    promote(TypeTree(tree.tpe).withSpan(tree.span))

  override def typedFunPart(fn: untpd.Tree, pt: Type)(implicit ctx: Context): Tree =
    typedExpr(fn, pt)

  override def typedBind(tree: untpd.Bind, pt: Type)(implicit ctx: Context): Bind = {
    assertTyped(tree)
    val body1 = typed(tree.body, pt)
    untpd.cpy.Bind(tree)(tree.name, body1).withType(tree.typeOpt)
  }

  override def typedUnApply(tree: untpd.UnApply, selType: Type)(implicit ctx: Context): UnApply = {
    val fun1 = {
      // retract PatternOrTypeBits like in typedExpr
      val ctx1 = ctx.retractMode(Mode.PatternOrTypeBits)
      typedUnadapted(tree.fun, AnyFunctionProto)(ctx1)
    }
    val implicits1 = tree.implicits.map(typedExpr(_))
    val patterns1 = tree.patterns.mapconserve(pat => typed(pat, pat.tpe))
    untpd.cpy.UnApply(tree)(fun1, implicits1, patterns1).withType(tree.tpe)
  }

  override def typedUnApply(tree: untpd.Apply, selType: Type)(implicit ctx: Context): Tree = {
    typedApply(tree, selType)
  }

  override def localDummy(cls: ClassSymbol, impl: untpd.Template)(implicit ctx: Context): Symbol = impl.symbol

  override def retrieveSym(tree: untpd.Tree)(implicit ctx: Context): Symbol = tree.symbol
  override def symbolOfTree(tree: untpd.Tree)(implicit ctx: Context): Symbol = tree.symbol

  override def localTyper(sym: Symbol): Typer = this

  override def index(trees: List[untpd.Tree])(implicit ctx: Context): Context = ctx

  override def tryInsertApplyOrImplicit(tree: Tree, pt: ProtoType, locked: TypeVars)(fallBack: => Tree)(implicit ctx: Context): Tree =
    fallBack

  override def tryNew[T >: Untyped <: Type]
    (treesInst: Instance[T])(tree: Trees.Tree[T], pt: Type, fallBack: => Tree)(implicit ctx: Context): Tree = fallBack

  override def completeAnnotations(mdef: untpd.MemberDef, sym: Symbol)(implicit ctx: Context): Unit = ()

  override def ensureConstrCall(cls: ClassSymbol, parents: List[Tree])(implicit ctx: Context): List[Tree] =
    parents

  override def handleUnexpectedFunType(tree: untpd.Apply, fun: Tree)(implicit ctx: Context): Tree = fun.tpe match {
    case mt: MethodType =>
      val args: List[Tree] = tree.args.zipWithConserve(mt.paramInfos)(typedExpr(_, _)).asInstanceOf[List[Tree]]
      assignType(untpd.cpy.Apply(tree)(fun, args), fun, args)
    case _ =>
      super.handleUnexpectedFunType(tree, fun)
  }

  override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): Tree =
    try super.typedUnadapted(tree, pt, locked)
    catch {
      case NonFatal(ex) =>
        if (ctx.isAfterTyper)
          println(i"exception while typing $tree of class ${tree.getClass} # ${tree.uniqueId}")
        throw ex
    }

  override def inlineExpansion(mdef: DefDef)(implicit ctx: Context): Tree = mdef

  override def checkVariance(tree: Tree)(implicit ctx: Context): Unit = ()
  override def inferView(from: Tree, to: Type)(implicit ctx: Context): Implicits.SearchResult =
    Implicits.NoMatchingImplicitsFailure
  override def checkCanEqual(ltp: Type, rtp: Type, span: Span)(implicit ctx: Context): Unit = ()
  override protected def addAccessorDefs(cls: Symbol, body: List[Tree])(implicit ctx: Context): List[Tree] = body
  override protected def checkEqualityEvidence(tree: tpd.Tree, pt: Type)(implicit ctx: Context): Unit = ()
  override protected def matchingApply(methType: MethodOrPoly, pt: FunProto)(implicit ctx: Context): Boolean = true
}
