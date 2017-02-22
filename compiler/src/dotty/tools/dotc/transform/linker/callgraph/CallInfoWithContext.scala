package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.transform.linker.summaries.{AbstractCallInfo, CallInfo}
import dotty.tools.dotc.transform.linker.types.JavaAllocatedType

import scala.collection.mutable

class CallInfoWithContext(val call: TermRef, val targs: List[Type], val argumentsPassed: List[Type], val outerTargs: OuterTargs)(
    val parent: Option[CallInfoWithContext], val callee: Option[CallInfo])(implicit ctx: Context) extends AbstractCallInfo {

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

  def isOnJavaAllocatedType: Boolean = call.prefix.isInstanceOf[JavaAllocatedType]

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: CallInfoWithContext =>
      call == obj.call && targs == obj.targs && argumentsPassed == obj.argumentsPassed && outerTargs == obj.outerTargs
    case _ => false
  }

  override def hashCode(): Int = java.util.Objects.hash(call, targs, argumentsPassed, outerTargs.mp)

}

object CallInfoWithContext {
  def apply(call: TermRef, targs: List[Type], argumentsPassed: List[Type], outerTargs: OuterTargs)(
      parent: Option[CallInfoWithContext], callee: Option[CallInfo])(implicit ctx: Context) = {
    new CallInfoWithContext(call, targs, argumentsPassed, outerTargs)(parent, callee)
  }
}
