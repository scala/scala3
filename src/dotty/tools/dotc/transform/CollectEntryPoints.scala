package dotty.tools.dotc.transform

import TreeTransforms._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.backend.jvm.isJavaEntryPoint
import dotty.tools.dotc.core._
import Symbols._
import scala.collection.SortedSet

class CollectEntryPoints extends TreeTransform {

  /** perform context-dependant initialization */
  override def init(implicit ctx: Context, info: TransformerInfo): Unit = {
    entryPoints = collection.immutable.TreeSet.empty[Symbol](new SymbolOrdering())
  }

  private var entryPoints:Set[Symbol] = _

  def getEntryPoints = entryPoints.toList

  override def name: String = "collectEntryPoints"
  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if(tree.symbol.owner.isClass && isJavaEntryPoint(tree.symbol, ctx)) {
      // collecting symbols for entry points here (as opposed to GenBCode where they are used)
      // has the advantage of saving an additional pass over all ClassDefs.
      entryPoints += tree.symbol
    }
    tree
  }
}

class SymbolOrdering(implicit ctx:Context) extends Ordering[Symbol] {
  override def compare(x: Symbol, y: Symbol): Int = {
    x.fullName.toString.compareTo(y.fullName.toString)
  }
}