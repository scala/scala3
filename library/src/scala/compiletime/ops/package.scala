package scala.compiletime

package object ops {
  object any {
    /** Equality comparison of two singleton types.
     * ```scala
     * val eq1: 1 == 1 = true
     * val eq2: 1 == "1" = false
     * val eq3: "1" == "1" = true
     * ```
     */
    type ==[X, Y] <: Boolean

    /** Inequality comparison of two singleton types.
     * ```scala
     * val eq1: 1 != 1 = false
     * val eq2: 1 != "1" = true
     * val eq3: "1" != "1" = false
     * ```
     */
    type !=[X, Y] <: Boolean
  }

  object string {
    /** Concatenation of two `String` singleton types.
     *  ```scala
     *  val hello: "hello " + "world" = "hello world"
     *  ```
     */
    type +[X <: String, Y <: String] <: String
  }

  object int {
    /** Addition of two `Int` singleton types.
     *  ```scala
     *  val sum: 2 + 2 = 4
     *  ```
     */
    type +[X <: Int, Y <: Int] <: Int

    /** Subtraction of two `Int` singleton types.
     *  ```scala
     *  val sub: 4 - 2 = 2
     *  ```
     */
    type -[X <: Int, Y <: Int] <: Int

    /** Multiplication of two `Int` singleton types.
     *  ```scala
     *  val mul: 4 * 2 = 8
     *  ```
     */
    type *[X <: Int, Y <: Int] <: Int

    /** Integer division of two `Int` singleton types.
     *  ```scala
     *  val div: 5 / 2 = 2
     *  ```
     */
    type /[X <: Int, Y <: Int] <: Int

    /** Remainder of the division of `X` by `Y`.
     *  ```scala
     *  val mod: 5 % 2 = 1
     *  ```
     */
    type %[X <: Int, Y <: Int] <: Int

    /** Binary left shift of `X` by `Y`.
     *  ```scala
     *  val lshift: 1 << 2 = 4
     *  ```
     */
    type <<[X <: Int, Y <: Int] <: Int

    /** Binary right shift of `X` by `Y`.
     *  ```scala
     *  val rshift: 10 >> 1 = 5
     *  ```
     */
    type >>[X <: Int, Y <: Int] <: Int

    /** Binary right shift of `X` by `Y`, filling the left with zeros.
     *  ```scala
     *  val rshiftzero: 10 >>> 1 = 5
     *  ```
     */
    type >>>[X <: Int, Y <: Int] <: Int

    /** Bitwise xor of `X` and `Y`.
     *  ```scala
     *  val xor: 10 ^ 30 = 20
     *  ```
     */
    type ^[X <: Int, Y <: Int] <: Int

    /** Less-than comparison of two `Int` singleton types.
     *  ```scala
     *  val lt1: 4 < 2 = false
     *  val lt2: 2 < 4 = true
     *  ```
     */
    type <[X <: Int, Y <: Int] <: Boolean

    /** Greater-than comparison of two `Int` singleton types.
     *  ```scala
     *  val gt1: 4 > 2 = true
     *  val gt2: 2 > 2 = false
     *  ```
     */
    type >[X <: Int, Y <: Int] <: Boolean

    /** Greater-or-equal comparison of two `Int` singleton types.
     *  ```scala
     *  val ge1: 4 >= 2 = true
     *  val ge2: 2 >= 3 = false
     *  ```
     */
    type >=[X <: Int, Y <: Int] <: Boolean

    /** Less-or-equal comparison of two `Int` singleton types.
     *  ```scala
     *  val lt1: 4 <= 2 = false
     *  val lt2: 2 <= 2 = true
     *  ```
     */
    type <=[X <: Int, Y <: Int] <: Boolean

    /** Bitwise and of `X` and `Y`.
     *  ```scala
     *  val and1: BitwiseAnd[4, 4] = 4
     *  val and2: BitwiseAnd[10, 5] = 0
     *  ```
     */
    type BitwiseAnd[X <: Int, Y <: Int] <: Int

    /** Bitwise or of `X` and `Y`.
     *  ```scala
     *  val or: BitwiseOr[10, 11] = 11
     *  ```
     */
    type BitwiseOr[X <: Int, Y <: Int] <: Int

    /** Absolute value of an `Int` singleton type.
     *  ```scala
     *  val abs: Abs[-1] = 1
     *  ```
     */
    type Abs[X <: Int] <: Int

    /** Negation of an `Int` singleton type.
     *  ```scala
     *  val neg1: Neg[-1] = 1
     *  val neg2: Neg[1] = -1
     *  ```
     */
    type Negate[X <: Int] <: Int

    /** Minimum of two `Int` singleton types.
     *  ```scala
     *  val min: Min[-1, 1] = -1
     *  ```
     */
    type Min[X <: Int, Y <: Int] <: Int

    /** Maximum of two `Int` singleton types.
     *  ```scala
     *  val max: Max[-1, 1] = 1
     *  ```
     */
    type Max[X <: Int, Y <: Int] <: Int

    /** String conversion of an `Int` singleton type.
     *  ```scala
     *  val abs: ToString[1] = "1"
     *  ```
     */
    type ToString[X <: Int] <: String
  }

  object boolean {

    /** Negation of a `Boolean` singleton type.
     *  ```scala
     *  val notFalse: ![false] = true
     *  val notTrue: ![true] = false
     *  ```
     */
    type ![X <: Boolean] <: Boolean

    /** Exclusive disjunction of two `Boolean` singleton types.
     * ```scala
     * val a: true ^ true = false
     * val b: false ^ true = true
     * ```
     */
    type ^[X <: Boolean, Y <: Boolean] <: Boolean

    /** Conjunction of two `Boolean` singleton types.
     *  ```scala
     *  val a: true && true = true
     *  val b: false && true = false
     *  ```
     */
    type &&[X <: Boolean, Y <: Boolean] <: Boolean

    /** Disjunction of two `Boolean` singleton types.
     * ```scala
     * val a: true || false = true
     * val b: false || false = false
     * ```
     */
    type ||[X <: Boolean, Y <: Boolean] <: Boolean
  }
}
