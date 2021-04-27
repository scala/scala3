package scala.compiletime
package ops

object any:
  /** Equality comparison of two singleton types.
   * ```scala
   * val eq1: 1 == 1 = true
   * val eq2: 1 == "1" = false
   * val eq3: "1" == "1" = true
   * ```
   * @syntax markdown
   */
  type ==[X, Y] <: Boolean

  /** Inequality comparison of two singleton types.
   * ```scala
   * val eq1: 1 != 1 = false
   * val eq2: 1 != "1" = true
   * val eq3: "1" != "1" = false
   * ```
   * @syntax markdown
   */
  type !=[X, Y] <: Boolean
