package dotty.tools.dotc
package typer

import core._
import Contexts._
import Types._
import Symbols._
import Decorators._
import typer.ProtoTypes._
import ast.{tpd, untpd}
import ast.Trees._
import scala.util.control.NonFatal
import util.Positions.Position
import config.Printers.typr

/** A version of Typer that keeps all symbols defined and referenced in a
 *  previously typed tree.
 *
 *  All definition nodes keep their symbols. All leaf nodes for idents, selects,
 *  and TypeTrees keep their types. Indexing is a no-op.
 *
 *  Otherwise, everything is as in Typer.
 */
class ReTyper extends Typer {
  import tpd._

  /** Checks that the given tree has been typed */
  protected def promote(tree: untpd.Tree)(implicit ctx: Context): tree.ThisTree[Type] = {
    assert(tree.hasType, i"$tree ${tree.getClass} ${tree.uniqueId}")
    tree.withType(tree.typeOpt)
  }

  override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree =
    promote(tree)

  override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
    assert(tree.hasType, tree)
    val qual1 = typed(tree.qualifier, AnySelectionProto)
    untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
  }

  override def typedLiteral(tree: untpd.Literal)(implicit ctc: Context): Literal =
    promote(tree)

  override def typedThis(tree: untpd.This)(implicit ctx: Context): Tree =
    promote(tree)

  override def typedSuper(tree: untpd.Super, pt: Type)(implicit ctx: Context): Tree =
    promote(tree)

  override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(implicit ctx: Context): TypeTree =
    promote(tree)

  override def typedBind(tree: untpd.Bind, pt: Type)(implicit ctx: Context): Bind = {
    assert(tree.hasType)
    val body1 = typed(tree.body, pt)
    untpd.cpy.Bind(tree)(tree.name, body1).withType(tree.typeOpt)
  }

  override def typedUnApply(tree: untpd.UnApply, selType: Type)(implicit ctx: Context): UnApply = {
    val fun1 = typedUnadapted(tree.fun, AnyFunctionProto)
    val implicits1 = tree.implicits.map(typedExpr(_))
    val patterns1 = tree.patterns.mapconserve(pat => typed(pat, pat.tpe))
    untpd.cpy.UnApply(tree)(fun1, implicits1, patterns1).withType(tree.tpe)
  }

  override def localDummy(cls: ClassSymbol, impl: untpd.Template)(implicit ctx: Context) = impl.symbol

  override def retrieveSym(tree: untpd.Tree)(implicit ctx: Context): Symbol = tree.symbol
  override def symbolOfTree(tree: untpd.Tree)(implicit ctx: Context): Symbol = tree.symbol

  override def localTyper(sym: Symbol) = this

  override def index(trees: List[untpd.Tree])(implicit ctx: Context) = ctx
  override def annotate(trees: List[untpd.Tree])(implicit ctx: Context) = ()

  override def tryInsertApplyOrImplicit(tree: Tree, pt: ProtoType)(fallBack: => Tree)(implicit ctx: Context): Tree =
    fallBack

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

  override def typedUnadapted(tree: untpd.Tree, pt: Type)(implicit ctx: Context) =
    try super.typedUnadapted(tree, pt)
    catch {
      case NonFatal(ex) =>
        if (ctx.isAfterTyper)
          println(i"exception while typing $tree of class ${tree.getClass} # ${tree.uniqueId}")
        throw ex
    }

  override def checkVariance(tree: Tree)(implicit ctx: Context) = ()
  override def inferView(from: Tree, to: Type)(implicit ctx: Context): Implicits.SearchResult =
    Implicits.NoImplicitMatches
  override def checkCanEqual(ltp: Type, rtp: Type, pos: Position)(implicit ctx: Context): Unit = ()
  override def inlineExpansion(mdef: DefDef)(implicit ctx: Context): List[Tree] = mdef :: Nil
}
