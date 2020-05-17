package dotty.tools
package dotc
package core
package typecomparer

import Decorators._
import Contexts._
import Types._
import Symbols._
import util.SimpleIdentityMap

final class GadtScopeState(
  private[typecomparer] var constraint: Constraint,
  private[typecomparer] var mapping: SimpleIdentityMap[Symbol, TypeVar],
  private[typecomparer] var reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
) {
  def this() = this(
    constraint = new OrderingConstraint(SimpleIdentityMap.Empty, SimpleIdentityMap.Empty, SimpleIdentityMap.Empty),
    mapping = SimpleIdentityMap.Empty,
    reverseMapping = SimpleIdentityMap.Empty
  )

  def isEmpty: Boolean = mapping.size == 0
  def nonEmpty: Boolean = !isEmpty

  def restore(other: GadtScopeState): Unit = {
    this.constraint = other.constraint
    this.mapping = other.mapping
    this.reverseMapping = other.reverseMapping
  }

  def fresh: GadtScopeState = new GadtScopeState(
    constraint,
    mapping,
    reverseMapping,
  )
}
