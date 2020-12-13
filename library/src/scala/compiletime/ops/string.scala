package scala.compiletime
package ops

object string:
  /** Concatenation of two `String` singleton types.
   *  ```scala
   *  val hello: "hello " + "world" = "hello world"
   *  ```
   */
  type +[X <: String, Y <: String] <: String
