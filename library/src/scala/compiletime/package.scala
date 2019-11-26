package scala

package object compiletime {

  erased def erasedValue[T]: T = ???

  /** The error method is used to produce user-defined compile errors during inline expansion.
   *  If an inline expansion results in a call error(msgStr) the compiler produces an error message containing the given msgStr.
   *
   *  ```scala
   *  error("My error message")
   *  ```
   *  or
   *  ```scala
   *  error(code"My error of this code: ${println("foo")}")
   *  ```
   */
  inline def error(inline msg: String): Nothing = ???

  /** Returns the string representations for code passed in the interpolated values
   *  ```scala
   *  inline def logged(p1: => Any) = {
   *    val c = code"code: $p1"
   *    val res = p1
   *    (c, p1)
   *  }
   *  logged(indentity("foo"))
   *  ```
   *  is equivalent to:
   *  ```scala
   *  ("code: indentity("foo")", indentity("foo"))
   *  ```
   */
  inline def (self: => StringContext) code (args: => Any*): String = ???

  inline def constValueOpt[T]: Option[T] = ???

  inline def constValue[T]: T = ???

  /** Summons first given matching one of the listed cases. E.g. in
   *
   *      given B { ... }
   *
   *      summonFrom {
   *        case given A => 1
   *        case given B => 2
   *        case given C => 3
   *        case _ => 4
   *      }
   *
   *  the returned value would be `2`.
   */
  inline def summonFrom[T](f: Nothing => T) <: T = ???

  /** Succesor of a natural number where zero is the type 0 and successors are reduced as if the definition was
   *
   *      type S[N <: Int] <: Int = N match {
   *        case 0 => 1
   *        case 1 => 2
   *        case 2 => 3
   *        ...
   *        case 2147483646 => 2147483647
   *      }
   */
  type S[N <: Int] <: Int

  /** Strip the Null type from `x`
   *
   *  ```
   *  val x: String|Null = ???
   *  val _: String = $notNull[String, x.type & String](x)
   *
   *  var y: String|Null = ???
   *  val _: String = $notNull[String, String](x)
   *  ```
   *
   *  Since `$notNull` is erased later, if `x.type` is a stable path,
   *  the type of `$notNull(x)` is also a stable path.
   */
  inline def $notNull[A, B](x: => A | Null): B =  x.asInstanceOf
}
