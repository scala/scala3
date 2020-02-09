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
    @infix type ==[+X <: AnyVal & Singleton, +Y <: AnyVal & Singleton] <: Boolean & Singleton

    /** Inequality comparison of two singleton types.
     * ```scala
     * val eq1: 1 != 1 = false
     * val eq2: 1 != "1" = true
     * val eq3: "1" != "1" = false
     * ```
     */
    @infix type !=[+X <: AnyVal & Singleton, +Y <: AnyVal & Singleton] <: Boolean & Singleton
  }

  object string {
    /** Concatenation of two `String` singleton types.
     *  ```scala
     *  val hello: "hello " + "world" = "hello world"
     *  ```
     */
    @infix type +[+X <: String & Singleton, +Y <: String & Singleton] <: String & Singleton
  }

  object int {
    /** Addition of two `Int` singleton types.
     *  ```scala
     *  val sum: 2 + 2 = 4
     *  ```
     */
    @infix type +[+X <: Int & Singleton, +Y <: Int & Singleton] <: Int & Singleton

    /** Subtraction of two `Int` singleton types.
     *  ```scala
     *  val sub: 4 - 2 = 2
     *  ```
     */
    @infix type -[+X <: Int & Singleton, +Y <: Int & Singleton] <: Int & Singleton

    /** Multiplication of two `Int` singleton types.
     *  ```scala
     *  val mul: 4 * 2 = 8
     *  ```
     */
    @infix type *[+X <: Int & Singleton, +Y <: Int & Singleton] <: Int & Singleton

    /** Integer division of two `Int` singleton types.
     *  ```scala
     *  val div: 5 / 2 = 2
     *  ```
     */
    @infix type /[+X <: Int & Singleton, +Y <: Int & Singleton] <: Int & Singleton

    /** Remainder of the division of `X` by `Y`.
    *  ```scala
     *  val mod: 5 % 2 = 1
     *  ```
     */
    @infix type %[+X <: Int & Singleton, +Y <: Int & Singleton] <: Int & Singleton

    /** Less-than comparison of two `Int` singleton types.
     *  ```scala
     *  val lt1: 4 < 2 = false
     *  val lt2: 2 < 4 = true
     *  ```
     */
    @infix type <[+X <: Int & Singleton, +Y <: Int & Singleton] <: Boolean & Singleton

    /** Greater-than comparison of two `Int` singleton types.
     *  ```scala
     *  val gt1: 4 > 2 = true
     *  val gt2: 2 > 2 = false
     *  ```
     */
    @infix type >[+X <: Int & Singleton, +Y <: Int & Singleton] <: Boolean & Singleton

    /** Greater-or-equal comparison of two `Int` singleton types.
     *  ```scala
     *  val ge1: 4 >= 2 = true
     *  val ge2: 2 >= 3 = false
     *  ```
     */
    @infix type >=[+X <: Int & Singleton, +Y <: Int & Singleton] <: Boolean & Singleton

    /** Less-or-equal comparison of two `Int` singleton types.
     *  ```scala
     *  val lt1: 4 <= 2 = false
     *  val lt2: 2 <= 2 = true
     *  ```
     */
    @infix type <=[+X <: Int & Singleton, +Y <: Int & Singleton] <: Boolean & Singleton

    /** Absolute value of an `Int` singleton type.
     *  ```scala
     *  val abs: Abs[-1] = 1
     *  ```
     */
    type Abs[+X <: Int & Singleton] <: Int & Singleton

    /** Negation of an `Int` singleton type.
     *  ```scala
     *  val neg1: Neg[-1] = 1
     *  val neg2: Neg[1] = -1
     *  ```
     */
    type Negate[+X <: Int & Singleton] <: Int & Singleton

    /** Minimum of two `Int` singleton types.
     *  ```scala
     *  val min: Min[-1, 1] = -1
     *  ```
     */
    type Min[+X <: Int & Singleton, +Y <: Int & Singleton] <: Int & Singleton

    /** Maximum of two `Int` singleton types.
     *  ```scala
     *  val abs: Abs[-1] = 1
     *  ```
     */
    type Max[+X <: Int & Singleton, +Y <: Int & Singleton] <: Int & Singleton

    /** String conversion of an `Int` singleton type.
     *  ```scala
     *  val abs: ToString[1] = "1"
     *  ```
     */
    type ToString[+X <: Int & Singleton] <: String & Singleton
  }

  object boolean {

    /** Negation of a `Boolean` singleton type.
     *  ```scala
     *  val notFalse: ![false] = true
     *  val notTrue: ![true] = false
     *  ```
     */
    type ![+X <: Boolean & Singleton] <: Boolean & Singleton

    /** Exclusive disjunction of two `Boolean` singleton types.
     * ```scala
     * val a: true ^ true = false
     * val b: false ^ true = true
     * ```
     */
    @infix type ^[+X <: Boolean & Singleton, +Y <: Boolean & Singleton] <: Boolean & Singleton

    /** Conjunction of two `Boolean` singleton types.
     *  ```scala
     *  val a: true && true = true
     *  val b: false && true = false
     *  ```
     */
    @infix type &&[+X <: Boolean & Singleton, +Y <: Boolean & Singleton] <: Boolean & Singleton

    /** Disjunction of two `Boolean` singleton types.
     * ```scala
     * val a: true || false = true
     * val b: false || false = false
     * ```
     */
    @infix type ||[+X <: Boolean & Singleton, +Y <: Boolean & Singleton] <: Boolean & Singleton
  }
}
