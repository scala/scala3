/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

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
