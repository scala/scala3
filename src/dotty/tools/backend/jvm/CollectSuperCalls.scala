package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** Collect all super calls except to the parent class.
 *
 *  This information is used to know if it is safe to remove a redundant mixin class.
 *  A redundant mixin class is one that is implemented by another mixin class. As the
 *  methods in a redundant mixin class could be implemented with a default abstract method,
 *  the redundant mixin class could be required as a parent by the JVM.
 */
class CollectSuperCalls extends MiniPhaseTransform {

  def phaseName: String = "collectSuperCalls"

  override def transformSuper(tree: Super)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree match {
      case Trees.Super(qual: This, mix) if mix.nonEmpty =>
        val classSymbol = qual.symbol.asClass.classSymbol
        registerSuperCall(classSymbol, tree.tpe.baseClasses.head)
      case _ =>
    }
    super.transformSuper(tree)
  }

  private def registerSuperCall(sym: ClassSymbol, calls: ClassSymbol)(implicit ctx: Context) = {
    ctx.genBCodePhase match {
      case genBCodePhase: GenBCode =>
        genBCodePhase.registerSuperCall(sym, calls)
      case _ =>
    }
  }
}
