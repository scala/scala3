package scala.compiletime
package ops

object int:
  /** Successor of a natural number where zero is the type 0 and successors are reduced as if the definition was:
   *
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  type S[N <: Int] <: Int = N match {
   *    case 0 => 1
   *    case 1 => 2
   *    case 2 => 3
   *    // ...
   *    case 2147483646 => 2147483647
   *  }
   *  ```
   *  @syntax markdown
   */
  type S[N <: Int] <: Int

  /** Addition of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val sum: 2 + 2 = 4
   *  ```
   *  @syntax markdown
   */
  infix type +[X <: Int, Y <: Int] <: Int

  /** Subtraction of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val sub: 4 - 2 = 2
   *  ```
   *  @syntax markdown
   */
  infix type -[X <: Int, Y <: Int] <: Int

  /** Multiplication of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val mul: 4 * 2 = 8
   *  ```
   *  @syntax markdown
   */
  infix type *[X <: Int, Y <: Int] <: Int

  /** Integer division of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val div: 5 / 2 = 2
   *  ```
   *  @syntax markdown
   */
  infix type /[X <: Int, Y <: Int] <: Int

  /** Remainder of the division of `X` by `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val mod: 5 % 2 = 1
   *  ```
   *  @syntax markdown
   */
  infix type %[X <: Int, Y <: Int] <: Int

  /** Binary left shift of `X` by `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val lshift: 1 << 2 = 4
   *  ```
   *  @syntax markdown
   */
  infix type <<[X <: Int, Y <: Int] <: Int

  /** Binary right shift of `X` by `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val rshift: 10 >> 1 = 5
   *  ```
   *  @syntax markdown
   */
  infix type >>[X <: Int, Y <: Int] <: Int

  /** Binary right shift of `X` by `Y`, filling the left with zeros.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val rshiftzero: 10 >>> 1 = 5
   *  ```
   *  @syntax markdown
   */
  infix type >>>[X <: Int, Y <: Int] <: Int

  /** Bitwise xor of `X` and `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val xor: 10 ^ 30 = 20
   *  ```
   *  @syntax markdown
   */
  infix type ^[X <: Int, Y <: Int] <: Int

  /** Less-than comparison of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val lt1: 4 < 2 = false
   *  val lt2: 2 < 4 = true
   *  ```
   *  @syntax markdown
   */
  infix type <[X <: Int, Y <: Int] <: Boolean

  /** Greater-than comparison of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val gt1: 4 > 2 = true
   *  val gt2: 2 > 2 = false
   *  ```
   *  @syntax markdown
   */
  infix type >[X <: Int, Y <: Int] <: Boolean

  /** Greater-or-equal comparison of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val ge1: 4 >= 2 = true
   *  val ge2: 2 >= 3 = false
   *  ```
   *  @syntax markdown
   */
  infix type >=[X <: Int, Y <: Int] <: Boolean

  /** Less-or-equal comparison of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val lt1: 4 <= 2 = false
   *  val lt2: 2 <= 2 = true
   *  ```
   *  @syntax markdown
   */
  infix type <=[X <: Int, Y <: Int] <: Boolean

  /** Bitwise and of `X` and `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val and1: BitwiseAnd[4, 4] = 4
   *  val and2: BitwiseAnd[10, 5] = 0
   *  ```
   *  @syntax markdown
   */
  type BitwiseAnd[X <: Int, Y <: Int] <: Int

  /** Bitwise or of `X` and `Y`.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val or: BitwiseOr[10, 11] = 11
   *  ```
   *  @syntax markdown
   */
  type BitwiseOr[X <: Int, Y <: Int] <: Int

  /** Absolute value of an `Int` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val abs: Abs[-1] = 1
   *  ```
   *  @syntax markdown
   */
  type Abs[X <: Int] <: Int

  /** Negation of an `Int` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val neg1: Negate[-1] = 1
   *  val neg2: Negate[1] = -1
   *  ```
   *  @syntax markdown
   */
  type Negate[X <: Int] <: Int

  /** Minimum of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val min: Min[-1, 1] = -1
   *  ```
   *  @syntax markdown
   */
  type Min[X <: Int, Y <: Int] <: Int

  /** Maximum of two `Int` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val max: Max[-1, 1] = 1
   *  ```
   *  @syntax markdown
   */
  type Max[X <: Int, Y <: Int] <: Int

  /** String conversion of an `Int` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val abs: ToString[1] = "1"
   *  ```
   *  @syntax markdown
   */
  @deprecated("Use compiletime.ops.any.ToString instead.","3.2.0")
  type ToString[X <: Int] <: String

  /** Long conversion of an `Int` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val x: ToLong[1] = 1L
   *  ```
   *  @syntax markdown
   */
  type ToLong[X <: Int] <: Long

  /** Float conversion of an `Int` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val x: ToFloat[1] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type ToFloat[X <: Int] <: Float

  /** Double conversion of an `Int` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val x: ToDouble[1] = 1.0
   *  ```
   *  @syntax markdown
   */
  type ToDouble[X <: Int] <: Double

  /** Number of zero bits preceding the highest-order ("leftmost")
   * one-bit in the two's complement binary representation of the specified `Int` singleton type.
   * Returns 32 if the specified singleton type has no one-bits in its two's complement representation,
   * in other words if it is equal to zero.
   *  ```scala
   *  //{
   *  import compiletime.ops.int.*
   *  //}
   *  val zero_lzc: NumberOfLeadingZeros[0] = 32
   *  val eight_lzc: NumberOfLeadingZeros[8] = 28
   *  type Log2[N <: Int] = 31 - NumberOfLeadingZeros[N]
   *  val log2of8: Log2[8] = 3
   *  ```
   *  @syntax markdown
   */
  type NumberOfLeadingZeros[X <: Int] <: Int
