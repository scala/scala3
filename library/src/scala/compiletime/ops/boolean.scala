package scala.compiletime
package ops

import language.experimental.captureChecking

object boolean:

  /** Negation of a `Boolean` singleton type.
  *  ```scala sc-hidden sc-name:ops-boolean-not-imports
  *  import compiletime.ops.boolean.*
  *  ```
  *  ```scala sc:compile sc-compile-with:ops-boolean-not-imports
   *  val notFalse: ![false] = true
   *  val notTrue: ![true] = false
   *  ```
   *  @syntax markdown
   */
  type ![X <: Boolean] <: Boolean

  /** Exclusive disjunction of two `Boolean` singleton types.
    *  ```scala sc-hidden sc-name:ops-boolean-xor-imports
    *  import compiletime.ops.boolean.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-boolean-xor-imports
   *  val a: true ^ true = false
   *  val b: false ^ true = true
   *  ```
   *  @syntax markdown
   */
  infix type ^[X <: Boolean, Y <: Boolean] <: Boolean

  /** Conjunction of two `Boolean` singleton types.
    *  ```scala sc-hidden sc-name:ops-boolean-and-imports
    *  import compiletime.ops.boolean.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-boolean-and-imports
   *  val a: true && true = true
   *  val b: false && true = false
   *  ```
   *  @syntax markdown
   */
  infix type &&[X <: Boolean, Y <: Boolean] <: Boolean

  /** Disjunction of two `Boolean` singleton types.
    *  ```scala sc-hidden sc-name:ops-boolean-or-imports
    *  import compiletime.ops.boolean.*
    *  ```
    *  ```scala sc:compile sc-compile-with:ops-boolean-or-imports
   *  val a: true || false = true
   *  val b: false || false = false
   *  ```
   *  @syntax markdown
   */
  infix type ||[X <: Boolean, Y <: Boolean] <: Boolean
