package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{PolyType, Type, TypeBounds}
import dotty.tools.dotc.transform.linker.types.ClosureType

trait AbstractCallInfo {

  /** This is type of method, that includes full type of receiver, eg: TermRef(receiver, Method) */
  val call: Type

  /** Type arguments at call site */
  val targs: List[Type]

  /** Type of the arguments at call site */
  val argumentsPassed: List[Type]

  def callSymbol(implicit ctx: Context): Symbol = call.normalizedPrefix match {
    case t: ClosureType => t.meth.meth.symbol
    case _ => call.termSymbol
  }
}

object AbstractCallInfo {

  def check(info: AbstractCallInfo)(implicit ctx: Context): Unit = {
    assert(info.call.termSymbol.isTerm)
    assert(info.callSymbol.isTerm)

    info.targs.foreach(targ => assert(!targ.isInstanceOf[TypeBounds], info.targs))

    info.call.widenDealias match {
      case t: PolyType => assert(t.paramNames.size == info.targs.size)
      case _ =>
    }
  }

}
