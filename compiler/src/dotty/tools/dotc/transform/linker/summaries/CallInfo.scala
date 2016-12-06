package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{PolyType, Type, TypeBounds}

case class CallInfo(call: Type, // this is type of method, that includes full type of reciever, eg: TermRef(reciever, Method)
                    targs: List[Type],
                    argumentsPassed: List[Type],
                    source: Option[CallInfo] = None // When source is not None this call was generated as part of a call to source
                   )(implicit ctx: Context) extends AbstractCallInfo {

  checkAbstractCallInfoAssertions()

}
