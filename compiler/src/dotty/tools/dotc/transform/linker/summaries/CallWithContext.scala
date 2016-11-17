package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type

import scala.collection.mutable

object CallWithContext {
  private[CallWithContext] var nextCallId = 0
}

class CallWithContext(call: Type, targs: List[Type], argumentsPassed: List[Type], val outerTargs: OuterTargs,
    val parent: CallWithContext, val callee: CallInfo)(implicit ctx: Context)
    extends CallInfo(call, targs, argumentsPassed) {

  import CallWithContext._

  val id = { nextCallId += 1; nextCallId }

  val outEdges = mutable.HashMap[CallInfo, List[CallWithContext]]().withDefault(x => Nil)

  override def hashCode(): Int = super.hashCode() ^ outerTargs.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: CallWithContext =>
        t.call == this.call && t.targs == this.targs && this.argumentsPassed == t.argumentsPassed &&
          this.outerTargs == t.outerTargs && this.source == t.source
      case _ => false
    }
  }

  override def toString: String = s"CallWithContext($call, $targs, $argumentsPassed, $outerTargs, $parent, $callee)"
}
