package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols.Symbol

case class CallGraph(entryPoints: Map[CallInfoWithContext, Int], reachableMethods: Set[CallInfoWithContext],
                     reachableTypes: Set[TypeWithContext], casts: Set[Cast], classOfs: Set[Symbol], outerMethods: Set[Symbol])(implicit ctx: Context) {

  private val reachableSet: Set[Symbol] =
    reachableMethods.map(x => x.call.termSymbol)

  private val reachableClassesSet: Set[Symbol] =
    reachableTypes.flatMap(x => x.tp.classSymbol :: x.tp.baseClasses)

  def isReachableMethod(sym: Symbol): Boolean = reachableSet.contains(sym)

  def isReachableClass(sym: Symbol): Boolean = reachableClassesSet.contains(sym)

  def isReachableClassOf(sym: Symbol): Boolean = classOfs.contains(sym)

}
