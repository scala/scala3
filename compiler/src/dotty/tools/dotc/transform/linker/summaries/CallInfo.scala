package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type

/* When source is not None this call was generated as part of a call to source */
case class CallInfo(call: Type, targs: List[Type], argumentsPassed: List[Type], source: Option[CallInfo] = None) extends AbstractCallInfo

object CallInfo {
  def check(info: CallInfo)(implicit ctx: Context): Unit = {
    AbstractCallInfo.check(info)
  }
}
