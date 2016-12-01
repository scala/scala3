package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{MethodicType, Type}

case class Cast(from: Type, to: Type)(implicit ctx: Context) {

  assert(!from.widenDealias.isInstanceOf[MethodicType], from.widenDealias)
  assert(!to.widenDealias.isInstanceOf[MethodicType], to.widenDealias)

  override def equals(other: Any): Boolean = {
    other match {
      case Cast(a, b) =>
        a =:= from && b =:= to
      case _ => false
    }
  }

  override def hashCode(): Int =
    from.typeSymbol.hashCode() * 31 + to.typeSymbol.hashCode()

  override def toString: String = s"Cast($from, $to)"
}
