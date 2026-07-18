package scala.compiletime
package ops

import language.experimental.captureChecking

object int:
  /** Successor of a natural number where zero is the type 0 and successors are reduced as if the definition was:
   *
  *  ```scala sc-hidden sc-name:ops-int-s-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-s-imports
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
  *  ```scala sc-hidden sc-name:ops-int-plus-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-plus-imports
   *  val sum: 2 + 2 = 4
   *  ```
   *  @syntax markdown
   */
  infix type +[X <: Int, Y <: Int] <: Int

  /** Subtraction of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-minus-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-minus-imports
   *  val sub: 4 - 2 = 2
   *  ```
   *  @syntax markdown
   */
  infix type -[X <: Int, Y <: Int] <: Int

  /** Multiplication of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-times-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-times-imports
   *  val mul: 4 * 2 = 8
   *  ```
   *  @syntax markdown
   */
  infix type *[X <: Int, Y <: Int] <: Int

  /** Integer division of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-div-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-div-imports
   *  val div: 5 / 2 = 2
   *  ```
   *  @syntax markdown
   */
  infix type /[X <: Int, Y <: Int] <: Int

  /** Remainder of the division of `X` by `Y`.
  *  ```scala sc-hidden sc-name:ops-int-mod-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-mod-imports
   *  val mod: 5 % 2 = 1
   *  ```
   *  @syntax markdown
   */
  infix type %[X <: Int, Y <: Int] <: Int

  /** Binary left shift of `X` by `Y`.
  *  ```scala sc-hidden sc-name:ops-int-lshift-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-lshift-imports
   *  val lshift: 1 << 2 = 4
   *  ```
   *  @syntax markdown
   */
  infix type <<[X <: Int, Y <: Int] <: Int

  /** Binary right shift of `X` by `Y`.
  *  ```scala sc-hidden sc-name:ops-int-rshift-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-rshift-imports
   *  val rshift: 10 >> 1 = 5
   *  ```
   *  @syntax markdown
   */
  infix type >>[X <: Int, Y <: Int] <: Int

  /** Binary right shift of `X` by `Y`, filling the left with zeros.
  *  ```scala sc-hidden sc-name:ops-int-urshift-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-urshift-imports
   *  val rshiftzero: 10 >>> 1 = 5
   *  ```
   *  @syntax markdown
   */
  infix type >>>[X <: Int, Y <: Int] <: Int

  /** Bitwise xor of `X` and `Y`.
  *  ```scala sc-hidden sc-name:ops-int-xor-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-xor-imports
   *  val xor: 10 ^ 30 = 20
   *  ```
   *  @syntax markdown
   */
  infix type ^[X <: Int, Y <: Int] <: Int

  /** Less-than comparison of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-lt-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-lt-imports
   *  val lt1: 4 < 2 = false
   *  val lt2: 2 < 4 = true
   *  ```
   *  @syntax markdown
   */
  infix type <[X <: Int, Y <: Int] <: Boolean

  /** Greater-than comparison of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-gt-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-gt-imports
   *  val gt1: 4 > 2 = true
   *  val gt2: 2 > 2 = false
   *  ```
   *  @syntax markdown
   */
  infix type >[X <: Int, Y <: Int] <: Boolean

  /** Greater-or-equal comparison of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-ge-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-ge-imports
   *  val ge1: 4 >= 2 = true
   *  val ge2: 2 >= 3 = false
   *  ```
   *  @syntax markdown
   */
  infix type >=[X <: Int, Y <: Int] <: Boolean

  /** Less-or-equal comparison of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-le-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-le-imports
   *  val lt1: 4 <= 2 = false
   *  val lt2: 2 <= 2 = true
   *  ```
   *  @syntax markdown
   */
  infix type <=[X <: Int, Y <: Int] <: Boolean

  /** Bitwise and of `X` and `Y`.
  *  ```scala sc-hidden sc-name:ops-int-bitwiseand-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-bitwiseand-imports
   *  val and1: BitwiseAnd[4, 4] = 4
   *  val and2: BitwiseAnd[10, 5] = 0
   *  ```
   *  @syntax markdown
   */
  type BitwiseAnd[X <: Int, Y <: Int] <: Int

  /** Bitwise or of `X` and `Y`.
  *  ```scala sc-hidden sc-name:ops-int-bitwiseor-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-bitwiseor-imports
   *  val or: BitwiseOr[10, 11] = 11
   *  ```
   *  @syntax markdown
   */
  type BitwiseOr[X <: Int, Y <: Int] <: Int

  /** Absolute value of an `Int` singleton type.
  *  ```scala sc-hidden sc-name:ops-int-abs-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-abs-imports
   *  val abs: Abs[-1] = 1
   *  ```
   *  @syntax markdown
   */
  type Abs[X <: Int] <: Int

  /** Negation of an `Int` singleton type.
  *  ```scala sc-hidden sc-name:ops-int-negate-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-negate-imports
   *  val neg1: Negate[-1] = 1
   *  val neg2: Negate[1] = -1
   *  ```
   *  @syntax markdown
   */
  type Negate[X <: Int] <: Int

  /** Minimum of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-min-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-min-imports
   *  val min: Min[-1, 1] = -1
   *  ```
   *  @syntax markdown
   */
  type Min[X <: Int, Y <: Int] <: Int

  /** Maximum of two `Int` singleton types.
  *  ```scala sc-hidden sc-name:ops-int-max-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-max-imports
   *  val max: Max[-1, 1] = 1
   *  ```
   *  @syntax markdown
   */
  type Max[X <: Int, Y <: Int] <: Int

  /** String conversion of an `Int` singleton type.
  *  ```scala sc-hidden sc-name:ops-int-tostring-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-tostring-imports
   *  val abs: ToString[1] = "1"
   *  ```
   *  @syntax markdown
   */
  @deprecated("Use compiletime.ops.any.ToString instead.","3.2.0")
  type ToString[X <: Int] <: String

  /** Long conversion of an `Int` singleton type.
  *  ```scala sc-hidden sc-name:ops-int-tolong-imports
   *  import compiletime.ops.int.*
   *  ```
   *  ```scala sc:compile sc-compile-with:ops-int-tolong-imports
   *  val x: ToLong[1] = 1L
   *  ```
   *  @syntax markdown
   */
  type ToLong[X <: Int] <: Long

  /** Float conversion of an `Int` singleton type.
    *  ```scala sc-hidden sc-name:ops-int-tofloat-imports
    *  import compiletime.ops.int.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-int-tofloat-imports
   *  val x: ToFloat[1] = 1.0f
   *  ```
   *  @syntax markdown
   */
  type ToFloat[X <: Int] <: Float

  /** Double conversion of an `Int` singleton type.
    *  ```scala sc-hidden sc-name:ops-int-todouble-imports
    *  import compiletime.ops.int.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-int-todouble-imports
   *  val x: ToDouble[1] = 1.0
   *  ```
   *  @syntax markdown
   */
  type ToDouble[X <: Int] <: Double

  /** Number of zero bits preceding the highest-order ("leftmost")
   * one-bit in the two's complement binary representation of the specified `Int` singleton type.
   * Returns 32 if the specified singleton type has no one-bits in its two's complement representation,
   * in other words if it is equal to zero.
    *  ```scala sc-hidden sc-name:ops-int-leadingzeros-imports
     *  import compiletime.ops.int.*
     *  ```
     *  ```scala sc:compile sc-compile-with:ops-int-leadingzeros-imports
   *  val zero_lzc: NumberOfLeadingZeros[0] = 32
   *  val eight_lzc: NumberOfLeadingZeros[8] = 28
   *  type Log2[N <: Int] = 31 - NumberOfLeadingZeros[N]
   *  val log2of8: Log2[8] = 3
   *  ```
   *  @syntax markdown
   */
  type NumberOfLeadingZeros[X <: Int] <: Int
