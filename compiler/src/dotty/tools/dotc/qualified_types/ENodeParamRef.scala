package dotty.tools.dotc.qualified_types
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Types.{CachedProxyType, SingletonType, Type}

/** Reference to the argument of an [[ENode.Lambda]].
 *
 *  Non-negative indices are de Bruijn indices bound by enclosing
 *  [[ENode.Lambda]] nodes (0 = innermost). Negative indices represent free
 *  variables, introduced when "opening" (similarly to "opening" in the context
 *  of the locally-nameless representation) a lambda during implication
 *  checking.
 *
 *  @param index De Bruijn index (>= 0) for bound variables, or a negative value
 *    for free variables
 *  @param underlying Underlying type of the argument
 */
final case class ENodeParamRef(index: Int, underlying: Type) extends CachedProxyType, SingletonType:
  override def underlying(using Context): Type = underlying
  override def computeHash(bs: Binders): Int = doHash(bs, index, underlying)
  def derivedENodeParamRef(index: Int, underlying: Type): ENodeParamRef =
    if index == this.index && (underlying eq this.underlying) then this
    else ENodeParamRef(index, underlying)
