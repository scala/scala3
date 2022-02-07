package dotty.tools.dotc
package transform

import core._
import Contexts._, Types._, MegaPhase._, ast.Trees._, Symbols._, Decorators._, Flags._

/** Transform references of the form
 *
 *     C.this.m
 *
 *  where `C` is a class with explicit self type and `C` is not a
 *  subclass of the owner of `m` to
 *
 *     C.this.asInstanceOf[S & C.this.type].m
 *
 *  where `S` is the self type of `C`.
 *  See run/i789.scala for a test case why this is needed.
 *
 *  Also replaces idents referring to the self type with ThisTypes.
 */
class ExplicitSelf extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = ExplicitSelf.name

  override def description: String = ExplicitSelf.description

  private def needsCast(tree: RefTree, cls: ClassSymbol)(using Context) =
    !cls.is(Package) && cls.givenSelfType.exists && !cls.derivesFrom(tree.symbol.owner)

  private def castQualifier(tree: RefTree, cls: ClassSymbol, thiz: Tree)(using Context) =
    cpy.Select(tree)(thiz.cast(AndType(cls.classInfo.selfType, thiz.tpe)), tree.name)

  override def transformIdent(tree: Ident)(using Context): Tree = tree.tpe match {
    case tp: ThisType =>
      report.debuglog(s"owner = ${ctx.owner}, context = ${ctx}")
      This(tp.cls).withSpan(tree.span)
    case TermRef(thisTp: ThisType, _) =>
      val cls = thisTp.cls
      if needsCast(tree, cls) then castQualifier(tree, cls, This(cls))
      else tree
    case _ =>
      tree
  }

  override def transformSelect(tree: Select)(using Context): Tree = tree match {
    case Select(thiz: This, name) if name.isTermName =>
      val cls = thiz.symbol.asClass
      if needsCast(tree, cls) then castQualifier(tree, cls, thiz)
      else tree
    case _ => tree
  }
}

object ExplicitSelf:
  val  name: String = "explicitSelf"
  val description: String = "make references to non-trivial self types explicit as casts"
