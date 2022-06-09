package scala.compiletime
package ops

object string:
  /** Concatenation of two `String` singleton types.
   *  ```scala
   *  //{
   *  import compiletime.ops.string._
   *  //}
   *  val hello: "hello " + "world" = "hello world"
   *  ```
   *  @syntax markdown
   */
  type +[+X <: String, +Y <: String] <: String

  /** Length of a `String` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.string._
   *  //}
   *  val helloSize: Length["hello"] = 5
   *  ```
   *  @syntax markdown
   */
  type Length[+X <: String] <: Int

  /** Substring of a `String` singleton type, with a singleton type
   * begin inclusive index `IBeg`, and a singleton type exclusive end index `IEnd`.
   * The substring begins at the specified IBeg and extends to the character at index IEnd - 1.
   * Thus the length of the substring is IEnd-IBeg.
   *  ```scala
   *  //{
   *  import compiletime.ops.string._
   *  //}
   *  val x: Substring["hamburger", 4, 8] = "urge"
   *  val y: Substring["smiles", 1, 5] = "mile"
   *  ```
   *  @syntax markdown
   */
  type Substring[+S <: String, +IBeg <: Int, +IEnd <: Int] <: String

  /** Tests if this `String` singleton type matches the given
   * regular expression `String` singleton type.
   *  ```scala
   *  //{
   *  import compiletime.ops.string._
   *  //}
   *  val x: Matches["unhappy", "un.*"] = true
   *  ```
   *  @syntax markdown
   */
  type Matches[+S <: String, +Regex <: String] <: Boolean

  /** Returns the Char type at the specified index.
   *  An index ranges from 0 to Length[S] - 1. The first Char of
   *  the sequence is at index 0, the next at index 1, and so on.
   *  ```scala
   *  //{
   *  import string._
   *  //}
   *  val c: CharAt["hello", 0] = 'h'
   *  ```
   *  @syntax markdown
   */
  type CharAt[+S <: String, +Idx <: Int] <: Char
