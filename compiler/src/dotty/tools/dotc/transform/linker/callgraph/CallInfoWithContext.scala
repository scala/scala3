package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.linker.summaries.{AbstractCallInfo, CallInfo}

import scala.collection.mutable

case class CallInfoWithContext(call: Type, targs: List[Type], argumentsPassed: List[Type], outerTargs: OuterTargs,
    parent: Option[CallInfoWithContext], callee: Option[CallInfo]) extends AbstractCallInfo {

  private val outEdges = mutable.HashMap[CallInfo, List[CallInfoWithContext]]().withDefault(x => Nil)

  def outEdgesIterator: Iterator[(CallInfo, List[CallInfoWithContext])] = outEdges.iterator

  def getOutEdges(callSite: CallInfo): List[CallInfoWithContext] = outEdges(callSite)

  def addOutEdges(callSite: CallInfo, edges: Traversable[CallInfoWithContext]): Unit = {
    var es = outEdges(callSite)
    for (e <- edges) {
      if (!es.contains(e))
        es = e :: es
    }
    outEdges(callSite) = es
  }

  def edgeCount: Int =
    outEdges.values.foldLeft(0)(_ + _.size)

}

object CallInfoWithContext {

  def check(info: CallInfoWithContext)(implicit ctx: Context): Unit = {
    AbstractCallInfo.check(info)
  }

}
