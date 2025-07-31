package dotty.tools.dotc.qualified_types
import dotty.tools.dotc.core.Types.{
  SingletonType,
  CachedProxyType,
  Type
}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Hashable.Binders


/** Reference to the argument of an [[ENode.Lambda]].
 *
 *  @param index 
 *    Debruijn index of the argument, starting from 0
 *  @param underyling
 *    Underlying type of the argument
 */
final case class ArgRefType(index: Int, underlying: Type) extends CachedProxyType, SingletonType:
  override def underlying(using Context): Type = underlying
  override def computeHash(bs: Binders): Int = doHash(bs, index, underlying)
