package dotty.tools.dotc
package transform
package linker

import scala.language.postfixOps

import TreeTransforms._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import dotty.tools.dotc.ast.{untpd, TreeTypeMap, tpd}
import dotty.tools.dotc.core
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.{TypeApplications, Flags}
import dotty.tools.dotc.typer.Applications
import dotty.tools.dotc.util.Positions
import typer.ErrorReporting._
import ast.Trees._
import Applications._
import TypeApplications._
import SymUtils._, core.NameOps._
import core.Mode

import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags

import scala.reflect.internal.util.Collections

class DeadCodeElimination extends MiniPhaseTransform {
  import tpd._

  def phaseName: String = "dce"

  private var reachableSet: Set[Symbol] = _
  private var reachableClassesSet: Set[Symbol] = _
  private var keepAfter: Phase = _
  private var exception: Tree = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    reachableSet = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph].getReachableMethods.map(x => x.call.termSymbol)
    reachableClassesSet = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph].getReachableTypes.flatMap(x => x.tp.classSymbol :: x.tp.baseClasses)
    keepAfter = ctx.phaseOfClass(classOf[BuildCallGraph])
    exception = Throw(New(ctx.requiredClassRef("dotty.runtime.DeadCodeEliminated"), Nil))
    this
  }


  override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    reachableSet = null
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): _root_.dotty.tools.dotc.ast.tpd.Tree = {
    val keepAsNew = tree.symbol.initial.validFor.firstPhaseId > keepAfter.period.phaseId
    if (tree.symbol.isConstructor || keepAsNew || reachableSet.contains(tree.symbol)) tree
    else tpd.cpy.DefDef(tree)(rhs = exception)
  }


  //TODO: drop classes that are unreachable with all their definitions and subclasses
//  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
//    val keepAsNew = tree.symbol.initial.validFor.firstPhaseId > keepAfter.period.phaseId
//    if (tree.symbol.isClass || keepAsNew || reachableClassesSet.contains(tree.symbol)) tree
//    else tpd.EmptyTree
//  }
//
//  override def transformApply(tree: _root_.dotty.tools.dotc.ast.tpd.Apply)(implicit ctx: Context, info: TransformerInfo): _root_.dotty.tools.dotc.ast.tpd.Tree = {
//    if (!tree.tpe.widenDealias.isInstanceOf[MethodicType] && tree.fun.symbol.isPrimaryConstructor) tree
//    else exception.ensureConforms(tree.tpe)
//  }
}
