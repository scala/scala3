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

  /** Tests if a type is a constant.
   * ```scala
   * val c1: IsConst[1] = true
   * val c2: IsConst["hi"] = true
   * val c3: IsConst[false] = true
   * ```
   * @syntax markdown
   */
  type IsConst[X] <: Boolean

  /** String conversion of a constant singleton type.
   *  ```scala
   *  val s1: ToString[1] = "1"
   *  val sTrue: ToString[true] = "true"
   *  ```
   *  @syntax markdown
   */
  type ToString[X] <: String