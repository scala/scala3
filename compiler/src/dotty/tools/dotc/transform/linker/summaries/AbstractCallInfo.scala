package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TermSymbol
import dotty.tools.dotc.core.Types.{PolyType, TermRef, Type, TypeBounds}
import dotty.tools.dotc.transform.linker.types.ClosureType
import dotty.tools.sharable

abstract class AbstractCallInfo(implicit ctx: Context) {

  targs.foreach(targ => assert(!targ.isInstanceOf[TypeBounds], targs))

  call.widenDealias match {
    case t: PolyType => assert(t.paramNames.size == targs.size, (t.paramNames, targs))
    case _ =>
  }

  final val id: Int = AbstractCallInfo.nextId()

  /** This is type of method, that includes full type of receiver, eg: TermRef(receiver, Method) */
  val call: TermRef

  /** Type arguments at call site */
  val targs: List[Type]

  /** Type of the arguments at call site */
  val argumentsPassed: List[Type]

  def callSymbol: TermSymbol = call.normalizedPrefix match {
    case t: ClosureType => t.meth.meth.symbol.asTerm
    case _ => call.termSymbol.asTerm
  }
}

object AbstractCallInfo {

  @sharable private var lastId = 0

  private[AbstractCallInfo] def nextId(): Int = {
    lastId += 1
    lastId
  }

}
