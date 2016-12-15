package dotty.tools.dotc.transform.linker.types

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{SingletonType, Type}
import dotty.tools.dotc.transform.linker.callgraph.OuterTargs

class ClosureType(val meth: tpd.Closure, val u: Type, val implementedMethod: Symbol, val outerTargs: OuterTargs)(implicit ctx: Context) extends SingletonType {
  assert(outerTargs.mp != null)

  /** The type to which this proxy forwards operations. */
  def underlying(implicit ctx: Context): Type = u

  /** customized hash code of this type.
    * NotCached for uncached types. Cached types
    * compute hash and use it as the type's hashCode.
    */
  override val hash: Int = implementedMethod.hashCode() + meth.meth.symbol.hashCode()

  override def hashCode() = hash

  override def equals(other: Any): Boolean = other match {
    case that: ClosureType =>
      meth == that.meth &&
        u == that.u &&
        implementedMethod == that.implementedMethod
    case _ => false
  }

  override def toString: String = s"ClosureType($meth, $u, $implementedMethod, $outerTargs)"
}
