package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.linker.types.TypeNormalizer

/* When source is not None this call was generated as part of a call to source */
class CallInfo private (val call: TermRef, val targs: List[Type], val argumentsPassed: List[Type],
    val source: Option[CallInfo]) extends AbstractCallInfo {

  override def equals(obj: Any): Boolean = obj match {
    case obj: CallInfo =>
      call == obj.call && targs == obj.targs && argumentsPassed == obj.argumentsPassed
    case _ => false
  }

  override def hashCode(): Int = call.hashCode ^ targs.hashCode ^ argumentsPassed.hashCode

}

object CallInfo {

  def apply(call: TermRef, targs: List[Type], argumentsPassed: List[Type], source: Option[CallInfo] = None)(implicit ctx: Context): CallInfo = {
    val normalizeType = new TypeNormalizer()
    val normalCall = normalizeType(call).asInstanceOf[TermRef]
    val normalTargs = targs.map(x => normalizeType(x))
    val normalArgumentsPassed = argumentsPassed.map(x => normalizeType(x))
    val callInfo = new CallInfo(normalCall, normalTargs, normalArgumentsPassed, source)
    AbstractCallInfo.assertions(callInfo)
    callInfo
  }

}
