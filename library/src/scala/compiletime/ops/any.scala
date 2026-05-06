package scala.compiletime
package ops

import language.experimental.captureChecking

object any:
  /** Equality comparison of two singleton types.
  * ```scala sc-hidden sc-name:ops-any-eq-imports
  * import compiletime.ops.any.*
  * ```
  * ```scala sc:compile sc-compile-with:ops-any-eq-imports
   * val eq1: 1 == 1 = true
   * val eq2: 1 == "1" = false
   * val eq3: "1" == "1" = true
   * ```
   * @syntax markdown
   */
  infix type ==[X, Y] <: Boolean

  /** Inequality comparison of two singleton types.
    * ```scala sc-hidden sc-name:ops-any-neq-imports
    * import compiletime.ops.any.*
    * ```
    * ```scala sc:compile sc-compile-with:ops-any-neq-imports
   * val eq1: 1 != 1 = false
   * val eq2: 1 != "1" = true
   * val eq3: "1" != "1" = false
   * ```
   * @syntax markdown
   */
  infix type !=[X, Y] <: Boolean

  /** Tests if a type is a constant.
   * ```scala sc-hidden sc-name:ops-any-isconst-imports
   * import compiletime.ops.any.*
   * ```
   * ```scala sc:compile sc-compile-with:ops-any-isconst-imports
   * val c1: IsConst[1] = true
   * val c2: IsConst["hi"] = true
   * val c3: IsConst[false] = true
   * val c4: IsConst[Any] = false
   * ```
   * If the type is not yet known, then `IsConst` remains unevaluated, and
   * will be evaluated only at its concrete type application. E.g.:
   * ```scala sc:compile sc-compile-with:ops-any-isconst-imports
   * //def `isConst`` returns the type `IsConst[X]`, since `X` is not yet known.
   * def isConst[X] : IsConst[X] = ???
   * val c5 : true = isConst[1] //now the type is known to be a constant
   * val c6 : false = isConst[Any] //now the type is known to be not a constant
   * ```
   * @syntax markdown
   */
  type IsConst[X] <: Boolean

  /** String conversion of a constant singleton type.
    *  ```scala sc-hidden sc-name:ops-any-tostring-imports
    *  import compiletime.ops.any.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-any-tostring-imports
   *  val s1: ToString[1] = "1"
   *  val sTrue: ToString[true] = "true"
   *  ```
   *  @syntax markdown
   */
  type ToString[X] <: String
