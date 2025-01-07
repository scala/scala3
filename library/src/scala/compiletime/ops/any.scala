package scala.compiletime
package ops

object any:
  /** Equality comparison of two singleton types.
   * ```scala
   *  //{
   *  import compiletime.ops.any.*
   *  //}
   * val eq1: 1 == 1 = true
   * val eq2: 1 == "1" = false
   * val eq3: "1" == "1" = true
   * ```
   * @syntax markdown
   */
  infix type ==[X, Y] <: Boolean

  /** Inequality comparison of two singleton types.
   * ```scala
   *  //{
   *  import compiletime.ops.any.*
   *  //}
   * val eq1: 1 != 1 = false
   * val eq2: 1 != "1" = true
   * val eq3: "1" != "1" = false
   * ```
   * @syntax markdown
   */
  infix type !=[X, Y] <: Boolean

  /** Tests if a type is a constant.
   * ```scala
   *  //{
   *  import compiletime.ops.any.*
   *  //}
   * val c1: IsConst[1] = true
   * val c2: IsConst["hi"] = true
   * val c3: IsConst[false] = true
   * val c4: IsConst[Any] = false
   * ```
   * If the type is not yet known, then `IsConst` remains unevaluated, and
   * will be evaluated only at its concrete type application. E.g.:
   * ```scala
   *  //{
   *  import compiletime.ops.any.*
   *  //}
   * //def `isConst`` returns the type `IsConst[X]`, since `X` is not yet known.
   * def isConst[X] : IsConst[X] = ???
   * val c5 : true = isConst[1] //now the type is known to be a constant
   * val c6 : false = isConst[Any] //now the type is known to be not a constant
   * ```
   * @syntax markdown
   */
  type IsConst[X] <: Boolean

  /** String conversion of a constant singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.any.*
   *  //}
   *  val s1: ToString[1] = "1"
   *  val sTrue: ToString[true] = "true"
   *  ```
   *  @syntax markdown
   */
  type ToString[X] <: String
