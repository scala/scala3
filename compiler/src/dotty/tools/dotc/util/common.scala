package dotty.tools.dotc
package util

import core.Types.WildcardType

/** Common values hoisted out for performance */
object common {

  val alwaysTrue = Function.const(true) _
  val alwaysFalse = Function.const(false) _
  val alwaysZero = Function.const(0) _
  val alwaysWildcardType = Function.const(WildcardType) _

}
