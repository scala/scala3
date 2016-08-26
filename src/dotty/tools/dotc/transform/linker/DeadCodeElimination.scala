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
  private var reachableSet: Set[Symbol] = null
  private var keepAfter: Phase = null
  private var exception: Tree = null

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    reachableSet = ctx.phaseOfClass(classOf[BuildCallGraph]).asInstanceOf[BuildCallGraph].getReachableMethods.map(x => x.call.termSymbol)
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
    else {
      tpd.cpy.DefDef(tree)(rhs = exception)
    }
  }

  //TODO: drop classes that are unreachable with all their definitions and subclasses
}
