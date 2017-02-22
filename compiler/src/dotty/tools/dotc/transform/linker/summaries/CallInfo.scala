package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{TermRef, Type}

/* When source is not None this call was generated as part of a call to source */
class CallInfo(val call: TermRef, val targs: List[Type], val argumentsPassed: List[Type],
    val source: Option[CallInfo])(implicit ctx: Context) extends AbstractCallInfo {

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: CallInfo =>
      call == obj.call && targs == obj.targs && argumentsPassed == obj.argumentsPassed && source == obj.source
    case _ => false
  }

  override def hashCode(): Int = java.util.Objects.hash(call, targs, argumentsPassed, source)

}

object CallInfo {
  def apply(call: TermRef, targs: List[Type], argumentsPassed: List[Type], source: Option[CallInfo] = None)(implicit ctx: Context) =
    new CallInfo(call, targs, argumentsPassed, source)
}
