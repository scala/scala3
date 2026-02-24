package dotty.tools.dotc
package util

/** Common values hoisted out for performance */
object common {

  val alwaysTrue: Any => Boolean = Function.const(true)
  val alwaysFalse: Any => Boolean = Function.const(false)
  val alwaysZero: Any => Int = Function.const(0)
}
