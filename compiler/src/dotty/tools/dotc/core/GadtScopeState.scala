package dotty.tools
package dotc
package core

import Types._
import Symbols._
import util.SimpleIdentityMap

final class GadtScopeState private(
  private[core] var constraint: Constraint,
  private[core] var mapping: SimpleIdentityMap[Symbol, TypeVar],
  private[core] var reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
) {
  def this() = this(
    constraint = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty),
    mapping = SimpleIdentityMap.empty,
    reverseMapping = SimpleIdentityMap.empty
  )

  def fresh: GadtScopeState = new GadtScopeState(
    constraint,
    mapping,
    reverseMapping
  )

  /** Restore the state from another [[GadtScopeState]], probably copied using [[fresh]] */
  def restore(other: GadtScopeState): Unit = {
    this.constraint = other.constraint
    this.mapping = other.mapping
    this.reverseMapping = other.reverseMapping
  }
}
