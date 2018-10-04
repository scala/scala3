package dotty.tools.dotc
package util

import core.Types.WildcardType

/** Common values hoisted out for performance */
object common {

  val alwaysTrue: Any => Boolean = Function.const(true) _
  val alwaysFalse: Any => Boolean = Function.const(false) _
  val alwaysZero: Any => Int = Function.const(0) _
  val alwaysWildcardType: Any => WildcardType.type = Function.const(WildcardType) _
}
