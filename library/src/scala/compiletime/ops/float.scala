package scala.compiletime
package ops

import language.experimental.captureChecking

object float:
  /** Addition of two `Float` singleton types.
  *  ```scala sc-hidden sc-name:ops-float-plus-imports
  *  import compiletime.ops.float.*
  *  ```
  *  ```scala sc:compile sc-compile-with:ops-float-plus-imports
   *  val sum: 2.0f + 2.0f = 4.0f
   *  ```
   *  @syntax markdown
   */
  infix type +[X <: Float, Y <: Float] <: Float

  /** Subtraction of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-minus-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-minus-imports
   *  val sub: 4.0f - 2.0f = 2.0f
   *  ```
   *  @syntax markdown
   */
  infix type -[X <: Float, Y <: Float] <: Float

  /** Multiplication of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-times-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-times-imports
   *  val mul: 4.0f * 2.0f = 8.0f
   *  ```
   *  @syntax markdown
   */
  infix type *[X <: Float, Y <: Float] <: Float

  /** Integer division of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-div-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-div-imports
   *  val div: 5.0f / 2.0f = 2.5f
   *  ```
   *  @syntax markdown
   */
  infix type /[X <: Float, Y <: Float] <: Float

  /** Remainder of the division of `X` by `Y`.
    *  ```scala sc-hidden sc-name:ops-float-mod-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-mod-imports
   *  val mod: 5.0f % 2.0f = 1.0f
   *  ```
   *  @syntax markdown
   */
  infix type %[X <: Float, Y <: Float] <: Float

  /** Less-than comparison of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-lt-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-lt-imports
   *  val lt1: 4.0f < 2.0f = false
   *  val lt2: 2.0f < 4.0f = true
   *  ```
   *  @syntax markdown
   */
  infix type <[X <: Float, Y <: Float] <: Boolean

  /** Greater-than comparison of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-gt-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-gt-imports
   *  val gt1: 4.0f > 2.0f = true
   *  val gt2: 2.0f > 2.0f = false
   *  ```
   *  @syntax markdown
   */
  infix type >[X <: Float, Y <: Float] <: Boolean

  /** Greater-or-equal comparison of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-ge-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-ge-imports
   *  val ge1: 4.0f >= 2.0f = true
   *  val ge2: 2.0f >= 3.0f = false
   *  ```
   *  @syntax markdown
   */
  infix type >=[X <: Float, Y <: Float] <: Boolean

  /** Less-or-equal comparison of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-le-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-le-imports
   *  val lt1: 4.0f <= 2.0f = false
   *  val lt2: 2.0f <= 2.0f = true
   *  ```
   *  @syntax markdown
   */
  infix type <=[X <: Float, Y <: Float] <: Boolean

  /** Absolute value of an `Float` singleton type.
    *  ```scala sc-hidden sc-name:ops-float-abs-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-abs-imports
   *  val abs: Abs[-1.0f] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type Abs[X <: Float] <: Float

  /** Negation of an `Float` singleton type.
    *  ```scala sc-hidden sc-name:ops-float-negate-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-negate-imports
   *  val neg1: Negate[-1.0f] = 1.0f
   *  val neg2: Negate[1.0f] = -1.0f
   *  ```
   *  @syntax markdown
   */
  type Negate[X <: Float] <: Float

  /** Minimum of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-min-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-min-imports
   *  val min: Min[-1.0f, 1.0f] = -1.0f
   *  ```
   *  @syntax markdown
   */
  type Min[X <: Float, Y <: Float] <: Float

  /** Maximum of two `Float` singleton types.
    *  ```scala sc-hidden sc-name:ops-float-max-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-max-imports
   *  val max: Max[-1.0f, 1.0f] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type Max[X <: Float, Y <: Float] <: Float

  /** Int conversion of a `Float` singleton type.
    *  ```scala sc-hidden sc-name:ops-float-toint-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-toint-imports
   *  val x: ToInt[1.0f] = 1
   *  ```
   *  @syntax markdown
   */
  type ToInt[X <: Float] <: Int

  /** Long conversion of a `Float` singleton type.
    *  ```scala sc-hidden sc-name:ops-float-tolong-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-tolong-imports
   *  val x: ToLong[1.0f] = 1L
   *  ```
   *  @syntax markdown
   */
  type ToLong[X <: Float] <: Long

  /** Double conversion of a `Float` singleton type.
    *  ```scala sc-hidden sc-name:ops-float-todouble-imports
    *  import compiletime.ops.float.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-float-todouble-imports
   *  val x: ToDouble[1.0f] = 1.0
   *  ```
   *  @syntax markdown
   */
  type ToDouble[X <: Float] <: Double
