package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type

case class Cast(from: Type, to: Type)(implicit ctx: Context) {
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
