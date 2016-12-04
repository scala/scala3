package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{PolyType, Type, TypeBounds}
import dotty.tools.dotc.transform.linker.types.ClosureType

trait AbstractCallInfo {

  protected def checkAbstractCallInfoAssertions()(implicit ctx: Context): Unit = {
    assert(call.termSymbol.isTerm)

    targs.foreach(targ => assert(!targ.isInstanceOf[TypeBounds], targs))

    call.widenDealias match {
      case t: PolyType => assert(t.paramNames.size == targs.size)
      case _ =>
    }
  }

  def call: Type

  def targs: List[Type]

  def argumentsPassed: List[Type]

  def callSymbol(implicit ctx: Context) = call.normalizedPrefix match {
    case t: ClosureType => t.meth.meth.symbol
    case _ => call.termSymbol
  }
}
