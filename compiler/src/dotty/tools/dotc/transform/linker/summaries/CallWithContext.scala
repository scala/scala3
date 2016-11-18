package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type

import scala.collection.mutable

class CallWithContext(call: Type, targs: List[Type], argumentsPassed: List[Type], val outerTargs: OuterTargs,
    val parent: CallWithContext, val callee: CallInfo)(implicit ctx: Context)
    extends CallInfo(call, targs, argumentsPassed) {

  val outEdges = mutable.HashMap[CallInfo, List[CallWithContext]]().withDefault(x => Nil)
  private val outEdgesOpt =
    if (trackOutEdges) Some(mutable.HashMap[CallInfo, List[CallWithContext]]().withDefault(x => Nil))
    else None

  def outEdgesIterator: Iterator[(CallInfo, List[CallWithContext])] =
    outEdgesOpt.fold[Iterator[(CallInfo, List[CallWithContext])]](Iterator.empty)(_.iterator)

  def getOutEdges(callSite: CallInfo): List[CallWithContext] =
    outEdgesOpt.fold(List.empty[CallWithContext])(_.apply(callSite))

  def addOutEdges(callSite: CallInfo, edges: Traversable[CallWithContext]): Unit = {
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

  override def hashCode(): Int = super.hashCode() ^ outerTargs.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: CallWithContext =>
        t.call == this.call && t.targs == this.targs && this.argumentsPassed == t.argumentsPassed &&
          this.outerTargs == t.outerTargs && this.source == t.source
      case _ => false
    }
  }

  private def trackOutEdges: Boolean =
    ctx.settings.linkVis.value

  override def toString: String = s"CallWithContext($call, $targs, $argumentsPassed, $outerTargs, $parent, $callee)"
}
