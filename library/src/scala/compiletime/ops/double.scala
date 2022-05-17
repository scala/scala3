package scala.compiletime
package ops

object double:
  /** Addition of two `Double` singleton types.
   *  ```scala
   *  val sum: 2.0 + 2.0 = 4.0
   *  ```
   *  @syntax markdown
   */
  type +[+X <: Double, +Y <: Double] <: Double

  /** Subtraction of two `Double` singleton types.
   *  ```scala
   *  val sub: 4.0 - 2.0 = 2.0
   *  ```
   *  @syntax markdown
   */
  type -[+X <: Double, +Y <: Double] <: Double

  /** Multiplication of two `Double` singleton types.
   *  ```scala
   *  val mul: 4.0 * 2.0 = 8.0
   *  ```
   *  @syntax markdown
   */
  type *[+X <: Double, +Y <: Double] <: Double

  /** Integer division of two `Double` singleton types.
   *  ```scala
   *  val div: 5.0 / 2.0 = 2.5
   *  ```
   *  @syntax markdown
   */
  type /[+X <: Double, +Y <: Double] <: Double

  /** Remainder of the division of `X` by `Y`.
   *  ```scala
   *  val mod: 5.0 % 2.0 = 1.0
   *  ```
   *  @syntax markdown
   */
  type %[+X <: Double, +Y <: Double] <: Double

  /** Less-than comparison of two `Double` singleton types.
   *  ```scala
   *  val lt1: 4.0 < 2.0 = false
   *  val lt2: 2.0 < 4.0 = true
   *  ```
   *  @syntax markdown
   */
  type <[+X <: Double, +Y <: Double] <: Boolean

  /** Greater-than comparison of two `Double` singleton types.
   *  ```scala
   *  val gt1: 4.0 > 2.0 = true
   *  val gt2: 2.0 > 2.0 = false
   *  ```
   *  @syntax markdown
   */
  type >[+X <: Double, +Y <: Double] <: Boolean

  /** Greater-or-equal comparison of two `Double` singleton types.
   *  ```scala
   *  val ge1: 4.0 >= 2.0 = true
   *  val ge2: 2.0 >= 3.0 = false
   *  ```
   *  @syntax markdown
   */
  type >=[+X <: Double, +Y <: Double] <: Boolean

  /** Less-or-equal comparison of two `Double` singleton types.
   *  ```scala
   *  val lt1: 4.0 <= 2.0 = false
   *  val lt2: 2.0 <= 2.0 = true
   *  ```
   *  @syntax markdown
   */
  type <=[+X <: Double, +Y <: Double] <: Boolean

  /** Absolute value of an `Double` singleton type.
   *  ```scala
   *  val abs: Abs[-1.0] = 1.0
   *  ```
   *  @syntax markdown
   */
  type Abs[+X <: Double] <: Double

  /** Negation of an `Double` singleton type.
   *  ```scala
   *  val neg1: Negate[-1.0] = 1.0
   *  val neg2: Negate[1.0] = -1.0
   *  ```
   *  @syntax markdown
   */
  type Negate[+X <: Double] <: Double

  /** Minimum of two `Double` singleton types.
   *  ```scala
   *  val min: Min[-1.0, 1.0] = -1.0
   *  ```
   *  @syntax markdown
   */
  type Min[+X <: Double, +Y <: Double] <: Double

  /** Maximum of two `Double` singleton types.
   *  ```scala
   *  val max: Max[-1.0, 1.0] = 1.0
   *  ```
   *  @syntax markdown
   */
  type Max[+X <: Double, +Y <: Double] <: Double

  /** Int conversion of a `Double` singleton type.
   *  ```scala
   *  val x: ToInt[1.0] = 1
   *  ```
   *  @syntax markdown
   */
  type ToInt[+X <: Double] <: Int

  /** Long conversion of a `Double` singleton type.
   *  ```scala
   *  val x: ToLong[1.0] = 1L
   *  ```
   *  @syntax markdown
   */
  type ToLong[+X <: Double] <: Long

  /** Float conversion of a `Double` singleton type.
   *  ```scala
   *  val x: ToFloat[1.0] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type ToFloat[+X <: Double] <: Float
