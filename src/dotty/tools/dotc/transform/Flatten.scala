package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import Symbols._
import Names.Name
import NameOps._
import typer.Mode
import Flags._
import SymUtils._
import SymDenotations.SymDenotation
import collection.mutable
import TreeTransforms.MiniPhaseTransform
import dotty.tools.dotc.transform.TreeTransforms.TransformerInfo

class Flatten extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import ast.tpd._
  override def phaseName = "flatten"

  /** Mark absent any companion classes or objects of the flattened version of `cls`
   *  in package `pkg` which are not yet completed. These are inner classes entered
   *  into the package scope by reading their class files. They should never be read
   *  because their information is superseded by the lifted class info.
   */
  private def invalidateUndefinedCompanions(pkg: ClassSymbol, cls: ClassSymbol)(implicit ctx: Context): Unit = {
    def invalidate(otherName: Name) = {
      val other = pkg.info.decl(otherName).asSymDenotation
      if (other.exists && !other.isCompleted) other.markAbsent
    }
    if (cls is Flags.Module)  {
      invalidate(cls.name.sourceModuleName)
      invalidate(cls.name.stripModuleClassSuffix.toTypeName)
    }
    else {
      invalidate(cls.name.toTermName)
      invalidate(cls.name.moduleClassName)
    }
  }

  def transformSym(ref: SymDenotation)(implicit ctx: Context) = {
    if (ref.isClass && !ref.is(Package) && !ref.owner.is(Package)) {
      val cls = ref.symbol.asClass
      val pkg = ref.enclosingPackageClass.asClass
      invalidateUndefinedCompanions(pkg, cls)(
         ctx.withPhase(cls.initial.validFor.phaseId).addMode(Mode.FutureDefsOK))      
      ref.copySymDenotation(
        name = ref.flatName(),
        owner = pkg)
    }
    else ref
  }

  private val liftedDefs = new mutable.ListBuffer[Tree]

  private def liftIfNested(tree: Tree)(implicit ctx: Context, info: TransformerInfo) =
    if (ctx.owner is Package) tree
    else {
      transformFollowing(tree).foreachInThicket(liftedDefs += _)
      EmptyTree
    }

  override def transformStats(stats: List[Tree])(implicit ctx: Context, info: TransformerInfo) =
    if (ctx.owner is Package) {
      val liftedStats = stats ++ liftedDefs
      liftedDefs.clear
      liftedStats
    }
    else stats

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo) =
    liftIfNested(tree)
}
