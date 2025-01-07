package scala.compiletime
package ops

object long:
  /** Successor of a natural number where zero is the type 0 and successors are reduced as if the definition was:
   *
   *  ```scala
   *  type S[N <: Long] <: Long = N match {
   *    case 0L => 1L
   *    case 1L => 2L
   *    case 2L => 3L
   *    // ...
   *    case 9223372036854775806L => 9223372036854775807L
   *  }
   *  ```
   *  @syntax markdown
   */
  @deprecated("The Successor feature for `Long` never worked, and after careful consideration it was decided to not support it at all.", "3.3.1")
  type S[N <: Long] <: Long

  /** Addition of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val sum: 2L + 2L = 4L
   *  ```
   *  @syntax markdown
   */
  infix type +[X <: Long, Y <: Long] <: Long

  /** Subtraction of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val sub: 4L - 2L = 2L
   *  ```
   *  @syntax markdown
   */
  infix type -[X <: Long, Y <: Long] <: Long

  /** Multiplication of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val mul: 4L * 2L = 8L
   *  ```
   *  @syntax markdown
   */
  infix type *[X <: Long, Y <: Long] <: Long

  /** Integer division of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val div: 5L / 2L = 2L
   *  ```
   *  @syntax markdown
   */
  infix type /[X <: Long, Y <: Long] <: Long

  /** Remainder of the division of `X` by `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val mod: 5L % 2L = 1L
   *  ```
   *  @syntax markdown
   */
  infix type %[X <: Long, Y <: Long] <: Long

  /** Binary left shift of `X` by `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val lshift: 1L << 2L = 4L
   *  ```
   *  @syntax markdown
   */
  infix type <<[X <: Long, Y <: Long] <: Long

  /** Binary right shift of `X` by `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val rshift: 10L >> 1L = 5L
   *  ```
   *  @syntax markdown
   */
  infix type >>[X <: Long, Y <: Long] <: Long

  /** Binary right shift of `X` by `Y`, filling the left with zeros.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val rshiftzero: 10L >>> 1L = 5L
   *  ```
   *  @syntax markdown
   */
  infix type >>>[X <: Long, Y <: Long] <: Long

  /** Bitwise xor of `X` and `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val xor: 10L ^ 30L = 20L
   *  ```
   *  @syntax markdown
   */
  infix type ^[X <: Long, Y <: Long] <: Long

  /** Less-than comparison of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val lt1: 4L < 2L = false
   *  val lt2: 2L < 4L = true
   *  ```
   *  @syntax markdown
   */
  infix type <[X <: Long, Y <: Long] <: Boolean

  /** Greater-than comparison of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val gt1: 4L > 2L = true
   *  val gt2: 2L > 2L = false
   *  ```
   *  @syntax markdown
   */
  infix type >[X <: Long, Y <: Long] <: Boolean

  /** Greater-or-equal comparison of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val ge1: 4L >= 2L = true
   *  val ge2: 2L >= 3L = false
   *  ```
   *  @syntax markdown
   */
  infix type >=[X <: Long, Y <: Long] <: Boolean

  /** Less-or-equal comparison of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val lt1: 4L <= 2L = false
   *  val lt2: 2L <= 2L = true
   *  ```
   *  @syntax markdown
   */
  infix type <=[X <: Long, Y <: Long] <: Boolean

  /** Bitwise and of `X` and `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val and1: BitwiseAnd[4L, 4L] = 4L
   *  val and2: BitwiseAnd[10L, 5L] = 0L
   *  ```
   *  @syntax markdown
   */
  type BitwiseAnd[X <: Long, Y <: Long] <: Long

  /** Bitwise or of `X` and `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val or: BitwiseOr[10L, 11L] = 11L
   *  ```
   *  @syntax markdown
   */
  type BitwiseOr[X <: Long, Y <: Long] <: Long

  /** Absolute value of an `Long` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val abs: Abs[-1L] = 1L
   *  ```
   *  @syntax markdown
   */
  type Abs[X <: Long] <: Long

  /** Negation of an `Long` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val neg1: Negate[-1L] = 1L
   *  val neg2: Negate[1L] = -1L
   *  ```
   *  @syntax markdown
   */
  type Negate[X <: Long] <: Long

  /** Minimum of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val min: Min[-1L, 1L] = -1L
   *  ```
   *  @syntax markdown
   */
  type Min[X <: Long, Y <: Long] <: Long

  /** Maximum of two `Long` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val max: Max[-1L, 1L] = 1L
   *  ```
   *  @syntax markdown
   */
  type Max[X <: Long, Y <: Long] <: Long

  /** Number of zero bits preceding the highest-order ("leftmost")
   * one-bit in the two's complement binary representation of the specified `Long` singleton type.
   * Returns 64 if the specified singleton type has no one-bits in its two's complement representation,
   * in other words if it is equal to zero.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val zero_lzc: NumberOfLeadingZeros[0L] = 64
   *  val eight_lzc: NumberOfLeadingZeros[8L] = 60
   *  type Log2[N <: Long] = int.-[63, NumberOfLeadingZeros[N]]
   *  val log2of8: Log2[8L] = 3
   *  ```
   *  @syntax markdown
   */
  type NumberOfLeadingZeros[X <: Long] <: Int

  /** Int conversion of a `Long` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val x: ToInt[1L] = 1
   *  ```
   *  @syntax markdown
   */
  type ToInt[X <: Long] <: Int

  /** Float conversion of a `Long` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val x: ToFloat[1L] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type ToFloat[X <: Long] <: Float

  /** Double conversion of a `Long` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.long.*
   *  //}
   *  val x: ToDouble[1L] = 1.0
   *  ```
   *  @syntax markdown
   */
  type ToDouble[X <: Long] <: Double
