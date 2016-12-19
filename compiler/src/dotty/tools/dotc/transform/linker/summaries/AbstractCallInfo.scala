package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{PolyType, Type, TypeBounds}
import dotty.tools.dotc.transform.linker.types.ClosureType
import dotty.tools.sharable

abstract class AbstractCallInfo(implicit ctx: Context) {

  assert(callSymbol.isTerm, callSymbol)

  targs.foreach(targ => assert(!targ.isInstanceOf[TypeBounds], targs))

  call.widenDealias match {
    case t: PolyType => assert(t.paramNames.size == targs.size, (t.paramNames, targs))
    case _ =>
  }

  final val id: Int = AbstractCallInfo.nextId()

  /** This is type of method, that includes full type of receiver, eg: TermRef(receiver, Method) */
  val call: Type

  /** Type arguments at call site */
  val targs: List[Type]

  /** Type of the arguments at call site */
  val argumentsPassed: List[Type]

  def callSymbol: Symbol = call.normalizedPrefix match {
    case t: ClosureType => t.meth.meth.symbol
    case _ => call.termSymbol
  }
}

object AbstractCallInfo {

  @sharable private var lastId = 0

  private[AbstractCallInfo] def nextId(): Int = {
    lastId += 1
    lastId
  }

}
