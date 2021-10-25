package scala.compiletime
package ops

import scala.annotation.experimental

object string:
  /** Concatenation of two `String` singleton types.
   *  ```scala
   *  val hello: "hello " + "world" = "hello world"
   *  ```
   *  @syntax markdown
   */
  type +[X <: String, Y <: String] <: String

  /** Length of a `String` singleton type.
   *  ```scala
   *  val helloSize: Size["hello"] = 5
   *  ```
   *  @syntax markdown
   */
  @experimental
  type Length[X <: String] <: Int

  /** Substring of a `String` singleton type, with a singleton type
   * begin inclusive index `IBeg`, and a singleton type exclusive end index `IEnd`.
   * The substring begins at the specified IBeg and extends to the character at index IEnd - 1.
   * Thus the length of the substring is IEnd-IBeg.
   *  ```scala
   *  val x: Substring["hamburger", 4, 8] = "urge"
   *  val y: Substring["smiles", 1, 5] = "mile"
   *  ```
   *  @syntax markdown
   */
  @experimental
  type Substring[S <: String, IBeg <: Int, IEnd <: Int] <: String

  /** Tests if this `String` singleton type matches the given
   * regular expression `String` singleton type.
   *  ```scala
   *  val x: Matches["unhappy", "un.*"] = true
   *  ```
   *  @syntax markdown
   */
  @experimental
  type Matches[S <: String, Regex <: String] <: Boolean
