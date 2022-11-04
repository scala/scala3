package scala.compiletime
package ops

object boolean:

  /** Negation of a `Boolean` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.boolean._
   *  //}
   *  val notFalse: ![false] = true
   *  val notTrue: ![true] = false
   *  ```
   *  @syntax markdown
   */
  type ![X <: Boolean] <: Boolean

  /** Exclusive disjunction of two `Boolean` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.boolean._
   *  //}
   *  val a: true ^ true = false
   *  val b: false ^ true = true
   *  ```
   *  @syntax markdown
   */
  type ^[X <: Boolean, Y <: Boolean] <: Boolean

  /** Conjunction of two `Boolean` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.boolean._
   *  //}
   *  val a: true && true = true
   *  val b: false && true = false
   *  ```
   *  @syntax markdown
   */
  type &&[X <: Boolean, Y <: Boolean] <: Boolean

  /** Disjunction of two `Boolean` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.boolean._
   *  //}
   *  val a: true || false = true
   *  val b: false || false = false
   *  ```
   *  @syntax markdown
   */
  type ||[X <: Boolean, Y <: Boolean] <: Boolean
