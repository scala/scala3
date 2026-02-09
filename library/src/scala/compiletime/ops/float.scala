package scala.compiletime
package ops

import language.experimental.captureChecking

object float:
  /** Addition of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val sum: 2.0f + 2.0f = 4.0f
   *  ```
   *  @syntax markdown
   */
  infix type +[X <: Float, Y <: Float] <: Float

  /** Subtraction of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val sub: 4.0f - 2.0f = 2.0f
   *  ```
   *  @syntax markdown
   */
  infix type -[X <: Float, Y <: Float] <: Float

  /** Multiplication of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val mul: 4.0f * 2.0f = 8.0f
   *  ```
   *  @syntax markdown
   */
  infix type *[X <: Float, Y <: Float] <: Float

  /** Integer division of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val div: 5.0f / 2.0f = 2.5f
   *  ```
   *  @syntax markdown
   */
  infix type /[X <: Float, Y <: Float] <: Float

  /** Remainder of the division of `X` by `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val mod: 5.0f % 2.0f = 1.0f
   *  ```
   *  @syntax markdown
   */
  infix type %[X <: Float, Y <: Float] <: Float

  /** Less-than comparison of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val lt1: 4.0f < 2.0f = false
   *  val lt2: 2.0f < 4.0f = true
   *  ```
   *  @syntax markdown
   */
  infix type <[X <: Float, Y <: Float] <: Boolean

  /** Greater-than comparison of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val gt1: 4.0f > 2.0f = true
   *  val gt2: 2.0f > 2.0f = false
   *  ```
   *  @syntax markdown
   */
  infix type >[X <: Float, Y <: Float] <: Boolean

  /** Greater-or-equal comparison of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val ge1: 4.0f >= 2.0f = true
   *  val ge2: 2.0f >= 3.0f = false
   *  ```
   *  @syntax markdown
   */
  infix type >=[X <: Float, Y <: Float] <: Boolean

  /** Less-or-equal comparison of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val lt1: 4.0f <= 2.0f = false
   *  val lt2: 2.0f <= 2.0f = true
   *  ```
   *  @syntax markdown
   */
  infix type <=[X <: Float, Y <: Float] <: Boolean

  /** Absolute value of an `Float` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val abs: Abs[-1.0f] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type Abs[X <: Float] <: Float

  /** Negation of an `Float` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val neg1: Negate[-1.0f] = 1.0f
   *  val neg2: Negate[1.0f] = -1.0f
   *  ```
   *  @syntax markdown
   */
  type Negate[X <: Float] <: Float

  /** Minimum of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val min: Min[-1.0f, 1.0f] = -1.0f
   *  ```
   *  @syntax markdown
   */
  type Min[X <: Float, Y <: Float] <: Float

  /** Maximum of two `Float` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val max: Max[-1.0f, 1.0f] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type Max[X <: Float, Y <: Float] <: Float

  /** Int conversion of a `Float` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val x: ToInt[1.0f] = 1
   *  ```
   *  @syntax markdown
   */
  type ToInt[X <: Float] <: Int

  /** Long conversion of a `Float` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val x: ToLong[1.0f] = 1L
   *  ```
   *  @syntax markdown
   */
  type ToLong[X <: Float] <: Long

  /** Double conversion of a `Float` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.float.*
   *  //}
   *  val x: ToDouble[1.0f] = 1.0
   *  ```
   *  @syntax markdown
   */
  type ToDouble[X <: Float] <: Double
