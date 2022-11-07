package dotty.tools.dotc
package util

import core.Types.WildcardType
import language.experimental.pureFunctions

/** Common values hoisted out for performance */
object common {

  val alwaysTrue: Any -> Boolean = Function.const(true)
  val alwaysFalse: Any -> Boolean = Function.const(false)
  val alwaysZero: Any -> Int = Function.const(0)
  val alwaysWildcardType: Any -> WildcardType.type = Function.const(WildcardType)
}
