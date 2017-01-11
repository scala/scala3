package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{PolyParam, Type}
import dotty.tools.dotc.transform.linker.summaries.{AbstractCallInfo, CallInfo}

import scala.collection.mutable

case class CallInfoWithContext(call: Type, targs: List[Type], argumentsPassed: List[Type], outerTargs: OuterTargs)
    (val parent: Option[CallInfoWithContext], val callee: Option[CallInfo])(implicit ctx: Context) extends AbstractCallInfo {

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

  def source: Option[CallInfo] = callee.flatMap(_.source)

}
