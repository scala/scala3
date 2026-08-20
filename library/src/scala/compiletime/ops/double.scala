package scala.compiletime
package ops

import language.experimental.captureChecking

object double:
  /** Addition of two `Double` singleton types.
  *  ```scala sc-hidden sc-name:ops-double-plus-imports
  *  import compiletime.ops.double.*
  *  ```
  *  ```scala sc:compile sc-compile-with:ops-double-plus-imports
   *  val sum: 2.0 + 2.0 = 4.0
   *  ```
   *  @syntax markdown
   */
  infix type +[X <: Double, Y <: Double] <: Double

  /** Subtraction of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-minus-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-minus-imports
   *  val sub: 4.0 - 2.0 = 2.0
   *  ```
   *  @syntax markdown
   */
  infix type -[X <: Double, Y <: Double] <: Double

  /** Multiplication of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-times-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-times-imports
   *  val mul: 4.0 * 2.0 = 8.0
   *  ```
   *  @syntax markdown
   */
  infix type *[X <: Double, Y <: Double] <: Double

  /** Integer division of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-div-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-div-imports
   *  val div: 5.0 / 2.0 = 2.5
   *  ```
   *  @syntax markdown
   */
  infix type /[X <: Double, Y <: Double] <: Double

  /** Remainder of the division of `X` by `Y`.
    *  ```scala sc-hidden sc-name:ops-double-mod-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-mod-imports
   *  val mod: 5.0 % 2.0 = 1.0
   *  ```
   *  @syntax markdown
   */
  infix type %[X <: Double, Y <: Double] <: Double

  /** Less-than comparison of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-lt-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-lt-imports
   *  val lt1: 4.0 < 2.0 = false
   *  val lt2: 2.0 < 4.0 = true
   *  ```
   *  @syntax markdown
   */
  infix type <[X <: Double, Y <: Double] <: Boolean

  /** Greater-than comparison of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-gt-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-gt-imports
   *  val gt1: 4.0 > 2.0 = true
   *  val gt2: 2.0 > 2.0 = false
   *  ```
   *  @syntax markdown
   */
  infix type >[X <: Double, Y <: Double] <: Boolean

  /** Greater-or-equal comparison of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-ge-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-ge-imports
   *  val ge1: 4.0 >= 2.0 = true
   *  val ge2: 2.0 >= 3.0 = false
   *  ```
   *  @syntax markdown
   */
  infix type >=[X <: Double, Y <: Double] <: Boolean

  /** Less-or-equal comparison of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-le-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-le-imports
   *  val lt1: 4.0 <= 2.0 = false
   *  val lt2: 2.0 <= 2.0 = true
   *  ```
   *  @syntax markdown
   */
  infix type <=[X <: Double, Y <: Double] <: Boolean

  /** Absolute value of an `Double` singleton type.
    *  ```scala sc-hidden sc-name:ops-double-abs-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-abs-imports
   *  val abs: Abs[-1.0] = 1.0
   *  ```
   *  @syntax markdown
   */
  infix type Abs[X <: Double] <: Double

  /** Negation of an `Double` singleton type.
    *  ```scala sc-hidden sc-name:ops-double-negate-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-negate-imports
   *  val neg1: Negate[-1.0] = 1.0
   *  val neg2: Negate[1.0] = -1.0
   *  ```
   *  @syntax markdown
   */
  type Negate[X <: Double] <: Double

  /** Minimum of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-min-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-min-imports
   *  val min: Min[-1.0, 1.0] = -1.0
   *  ```
   *  @syntax markdown
   */
  type Min[X <: Double, Y <: Double] <: Double

  /** Maximum of two `Double` singleton types.
    *  ```scala sc-hidden sc-name:ops-double-max-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-max-imports
   *  val max: Max[-1.0, 1.0] = 1.0
   *  ```
   *  @syntax markdown
   */
  type Max[X <: Double, Y <: Double] <: Double

  /** Int conversion of a `Double` singleton type.
    *  ```scala sc-hidden sc-name:ops-double-toint-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-toint-imports
   *  val x: ToInt[1.0] = 1
   *  ```
   *  @syntax markdown
   */
  type ToInt[X <: Double] <: Int

  /** Long conversion of a `Double` singleton type.
    *  ```scala sc-hidden sc-name:ops-double-tolong-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-tolong-imports
   *  val x: ToLong[1.0] = 1L
   *  ```
   *  @syntax markdown
   */
  type ToLong[X <: Double] <: Long

  /** Float conversion of a `Double` singleton type.
    *  ```scala sc-hidden sc-name:ops-double-tofloat-imports
    *  import compiletime.ops.double.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-double-tofloat-imports
   *  val x: ToFloat[1.0] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type ToFloat[X <: Double] <: Float
