package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
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
  import tpd._

  def phaseName: String = "collectSuperCalls"

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.qualifier match {
      case Super(qual, mix) if mix.nonEmpty =>
        val classSymbol: ClassSymbol = qual match {
          case qual: This => qual.symbol.asClass.classSymbol
          case qual => qual.symbol.info.classSymbol.asClass
        }
        registerSuperCall(classSymbol, tree.symbol.owner.asClass)
      case _ =>
    }
    tree
  }

  private def registerSuperCall(sym: ClassSymbol, calls: ClassSymbol)(implicit ctx: Context) = {
    ctx.genBCodePhase match {
      case genBCodePhase: GenBCode =>
        genBCodePhase.registerSuperCall(sym, calls)
      case _ =>
    }
  }
}
