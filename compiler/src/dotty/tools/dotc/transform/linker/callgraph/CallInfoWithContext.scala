package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{PolyType, Type}
import dotty.tools.dotc.transform.linker.summaries.{AbstractCallInfo, CallInfo}

import scala.collection.mutable

case class CallInfoWithContext(call: Type, targs: List[Type], argumentsPassed: List[Type], outerTargs: OuterTargs,
    parent: Option[CallInfoWithContext], callee: Option[CallInfo])(implicit ctx: Context) extends AbstractCallInfo {

  checkAbstractCallInfoAssertions()
  assert(parent != null)
  assert(callee != null)
  // FIXME: assert(!targs.exists(_.widenDealias.isInstanceOf[PolyType]), targs)

  val outEdges = mutable.HashMap[CallInfo, List[CallInfoWithContext]]().withDefault(x => Nil)
  private val outEdgesOpt =
    if (trackOutEdges) Some(mutable.HashMap[CallInfo, List[CallInfoWithContext]]().withDefault(x => Nil))
    else None

  def outEdgesIterator: Iterator[(CallInfo, List[CallInfoWithContext])] =
    outEdgesOpt.fold[Iterator[(CallInfo, List[CallInfoWithContext])]](Iterator.empty)(_.iterator)

  def getOutEdges(callSite: CallInfo): List[CallInfoWithContext] =
    outEdgesOpt.fold(List.empty[CallInfoWithContext])(_.apply(callSite))

  def addOutEdges(callSite: CallInfo, edges: Traversable[CallInfoWithContext]): Unit = {
    outEdgesOpt.foreach { outEdges =>
      var es = outEdges(callSite)
      for (e <- edges) {
        if (!es.contains(e))
          es = e :: es
      }
      outEdges(callSite) = es
    }
  }

  def edgeCount: Int =
    outEdgesOpt.fold(0)(_.values.foldLeft(0)(_ + _.size))

  private def trackOutEdges: Boolean =
    ctx.settings.linkVis.value

}
