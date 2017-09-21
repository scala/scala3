package dotty.tools.dotc
package transform

import core._
import DenotTransformers.InfoTransformer
import Contexts.Context
import Symbols._
import TreeTransforms.MiniPhaseTransform
import Flags._
import Decorators._
import TreeTransforms.TransformerInfo
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Types._

/** Remove method definitions that return phantom values */
class PhantomDefs extends MiniPhaseTransform with InfoTransformer { thisTransform =>

  override def phaseName = "phantomDefs"

  override def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set(classOf[Constructors])

  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    assert(!tree.symbol.is(Method) || !isPhantomMethod(tree.symbol))
  }

  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val newBody = tree.body.filterConserve(t => !isPhantomMethod(t.symbol))
    cpy.Template(tree)(body = newBody)
  }

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp: ClassInfo if !sym.is(JavaDefined) && !sym.is(Scala2x) && tp.decls.exists(isPhantomMethod) =>
      val newDecls = tp.decls.filteredScope(decl => !isPhantomMethod(decl))
      tp.derivedClassInfo(decls = newDecls)
    case _ => tp
  }

  private def isPhantomMethod(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.info.finalResultType.widenDealias.classSymbol eq defn.ErasedPhantomClass

}

