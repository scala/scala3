package scala.compiletime

import scala.annotation.infix

package object ops {
  object any {
    /** Equality comparison of two singleton types.
     * ```scala
     * val eq1: 1 == 1 = true
     * val eq2: 1 == "1" = false
     * val eq3: "1" == "1" = true
     * ```
     */
    @infix type ==[X <: AnyVal, Y <: AnyVal] <: Boolean

    /** Inequality comparison of two singleton types.
     * ```scala
     * val eq1: 1 != 1 = false
     * val eq2: 1 != "1" = true
     * val eq3: "1" != "1" = false
     * ```
     */
    @infix type !=[X <: AnyVal, Y <: AnyVal] <: Boolean
  }

  object string {
    /** Concatenation of two `String` singleton types.
     *  ```scala
     *  val hello: "hello " + "world" = "hello world"
     *  ```
     */
    @infix type +[X <: String, Y <: String] <: String
  }

  object int {
    /** Addition of two `Int` singleton types. Type-level equivalent of `scala.Int.+`
     *  ```scala
     *  val sum: 2 + 2 = 4
     *  ```
     */
    @infix type +[X <: Int, Y <: Int] <: Int

    /** Subtraction of two `Int` singleton types. Type-level equivalent of `scala.Int.-`
     *  ```scala
     *  val sub: 4 - 2 = 2
     *  ```
     */
    @infix type -[X <: Int, Y <: Int] <: Int

    /** Multiplication of two `Int` singleton types. Type-level equivalent of `scala.Int.*`
     *  ```scala
     *  val mul: 4 * 2 = 8
     *  ```
     */
    @infix type *[X <: Int, Y <: Int] <: Int

    /** Integer division of two `Int` singleton types. Type-level equivalent of `scala.Int./`
     *  ```scala
     *  val div: 5 / 2 = 2
     *  ```
     */
    @infix type /[X <: Int, Y <: Int] <: Int

    /** Remainder of the division of `X` by `Y`. Type-level equivalent of `scala.Int.%`
     *  ```scala
     *  val mod: 5 % 2 = 1
     *  ```
     */
    @infix type %[X <: Int, Y <: Int] <: Int

    /** Less-than comparison of two `Int` singleton types. Type-level equivalent of `scala.Int.<`.
     *  ```scala
     *  val lt1: 4 < 2 = false
     *  val lt2: 2 < 4 = true
     *  ```
     */
    @infix type <[X <: Int, Y <: Int] <: Boolean

    /** Greater-than comparison of two `Int` singleton types. Type-level equivalent of `scala.Int.>`.
     *  ```scala
     *  val gt1: 4 > 2 = true
     *  val gt2: 2 > 2 = false
     *  ```
     */
    @infix type >[X <: Int, Y <: Int] <: Boolean

    /** Greater-or-equal comparison of two `Int` singleton types. Type-level equivalent of `scala.Int.>=`.
     *  ```scala
     *  val ge1: 4 >= 2 = true
     *  val ge2: 2 >= 3 = false
     *  ```
     */
    @infix type >=[X <: Int, Y <: Int] <: Boolean

    /** Less-or-equal comparison of two `Int` singleton types. Type-level equivalent of `scala.Int.<=`.
     *  ```scala
     *  val lt1: 4 <= 2 = false
     *  val lt2: 2 <= 2 = true
     *  ```
     */
    @infix type <=[X <: Int, Y <: Int] <: Boolean

    /** Absolute value of an `Int` singleton type. Type-level equivalent of `scala.Int.abs`.
     *  ```scala
     *  val abs: Abs[-1] = 1
     *  ```
     */
    type Abs[X <: Int] <: Int

    /** Negation of an `Int` singleton type. Type-level equivalent of `scala.Int.unary_-`.
     *  ```scala
     *  val neg1: Neg[-1] = 1
     *  val neg2: Neg[1] = -1
     *  ```
     */
    type Negate[X <: Int] <: Int

    /** Minimum of two `Int` singleton types. Type-level equivalent of `scala.Int.min`.
     *  ```scala
     *  val min: Min[-1, 1] = -1
     *  ```
     */
    type Min[X <: Int, Y <: Int] <: Int

    /** Maximum of two `Int` singleton types. Type-level equivalent of `scala.Int.max`.
     *  ```scala
     *  val abs: Abs[-1] = 1
     *  ```
     */
    type Max[X <: Int, Y <: Int] <: Int

    /** String conversion of an `Int` singleton type. Type-level equivalent of `scala.Int.toString`.
     *  ```scala
     *  val abs: ToString[1] = "1"
     *  ```
     */
    type ToString[X <: Int] <: String
  }

  object boolean {

    /** Negation of a `Boolean` singleton type. Type-level equivalent of `scala.Boolean.unary_!`.
     *  ```scala
     *  val notFalse: ![false] = true
     *  val notTrue: ![true] = false
     *  ```
     */
    type ![X <: Boolean] <: Boolean

    /** Exclusive disjunction of two `Boolean` singleton types. Type-level equivalent of `scala.Boolean.^`.
     * ```scala
     * val a: true ^ true = false
     * val b: false ^ true = true
     * ```
     */
    @infix type ^[X <: Boolean, Y <: Boolean] <: Boolean

    /** Conjunction of two `Boolean` singleton types. Type-level equivalent of `scala.Boolean.&&`.
     *  ```scala
     *  val a: true && true = true
     *  val b: false && true = false
     *  ```
     */
    @infix type &&[X <: Boolean, Y <: Boolean] <: Boolean

    /** Disjunction of two `Boolean` singleton types. Type-level equivalent of `scala.Boolean.||`.
     * ```scala
     * val a: true || false = true
     * val b: false || false = false
     * ```
     */
    @infix type ||[X <: Boolean, Y <: Boolean] <: Boolean
  }
}
